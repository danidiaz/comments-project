{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Comments.Main (appMain, manuallyWiredAppMain) where

import Bean.Current
import Bean.JsonConf
import Bean.JsonConf.YamlFile qualified
import Bean.Sqlite.Pool
import Bean.ThreadLocal
import Cauldron
import Cauldron.Builder
import Cauldron.Managed
import Comments.Repository
import Comments.Repository.Sqlite qualified
import Comments.Runner
import Comments.Server
import Control.Exception (throwIO)
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Data.Function ((&))
import Data.Functor.Identity
import Log
import Log.Backend.StandardOutput
import Servant.Server (Handler)
import Sqlite (Connection)

appMain :: IO ()
appMain = do
  let depGraph = getDependencyGraph cauldron
  writeAsDot (defaultStyle Nothing) "beans.dot" $ collapseToPrimaryBeans $ removeDecos $ removeSecondaryBeans $ depGraph
  cook forbidDepCycles cauldron & either throwIO \action -> with action \beans -> do
    case taste beans of
      Nothing -> error "no bean found"
      Just Runner {runServer} -> runServer

cauldron :: Cauldron Managed
cauldron =
  [ let makeJsonConf = Bean.JsonConf.YamlFile.make $ Bean.JsonConf.YamlFile.loadYamlSettings ["conf.yaml"] [] Bean.JsonConf.YamlFile.useEnv
     in recipe @JsonConf $ effIO $ wire makeJsonConf,
    recipe @Logger $ eff $ wire $ managed withStdOutLogger,
    recipe @SqlitePoolConf $ effIO $ wire $ Bean.JsonConf.lookupSection @SqlitePoolConf "sqlite",
    recipe @SqlitePool $ eff $ wire \conf -> managed $ Bean.Sqlite.Pool.make conf,
    recipe @(ThreadLocal Connection) $ effIO $ wire makeThreadLocal,
    recipe @(Current Connection) $ val $ wire makeThreadLocalCurrent,
    recipe @CommentsRepository $ val $ wire Comments.Repository.Sqlite.make,
    recipe @CommentsServer $ val $ wire makeCommentsServer,
    recipe @RunnerConf $ effIO $ wire $ Bean.JsonConf.lookupSection @RunnerConf "runner",
    recipe @Runner $ val $ wire $ makeRunner
  ]
  where
    effIO x = hoistConstructor liftIO $ eff x

manuallyWired :: Managed Runner
manuallyWired = do
  jsonConf <-
    let makeJsonConf = Bean.JsonConf.YamlFile.make $ Bean.JsonConf.YamlFile.loadYamlSettings ["conf.yaml"] [] Bean.JsonConf.YamlFile.useEnv
     in liftIO $ makeJsonConf
  logger <- managed withStdOutLogger
  sqlitePoolConf <- liftIO $ Bean.JsonConf.lookupSection @SqlitePoolConf "sqlite" jsonConf
  sqlitePool <- managed $ Bean.Sqlite.Pool.make sqlitePoolConf
  threadLocal <- liftIO makeThreadLocal
  let currentConnection = makeThreadLocalCurrent threadLocal
  let commentsRepository = Comments.Repository.Sqlite.make logger currentConnection
  let commentsServer = makeCommentsServer logger commentsRepository
  runnerConf <- liftIO $ Bean.JsonConf.lookupSection @RunnerConf "runner" jsonConf
  pure $ makeRunner runnerConf sqlitePool threadLocal logger commentsServer

manuallyWiredAppMain :: IO ()
manuallyWiredAppMain = do
  with manuallyWired \Runner {runServer} -> runServer

polymorphicallyWired :: (MonadBuilder m, ConstructorMonad m ~ Managed) => m (ArgsApplicative m Runner)
polymorphicallyWired = do
  jsonConf <- do
    let makeJsonConf = Bean.JsonConf.YamlFile.make $ Bean.JsonConf.YamlFile.loadYamlSettings ["conf.yaml"] [] Bean.JsonConf.YamlFile.useEnv
    addIOEff_ $ pure makeJsonConf
  logger <- addEff_ $ pure $ managed withStdOutLogger
  sqlitePoolConf <- addIOEff_ $ Bean.JsonConf.lookupSection @SqlitePoolConf "sqlite" <$> jsonConf
  sqlitePool <- addEff_ $ (\conf -> managed $ Bean.Sqlite.Pool.make conf) <$> sqlitePoolConf
  threadLocal <- addIOEff_ $ pure $ makeThreadLocal
  currentConnection <- addVal_ $ makeThreadLocalCurrent <$> threadLocal
  commentsRepository <- addVal_ $ Comments.Repository.Sqlite.make <$> logger <*> currentConnection
  commentsServer <- addVal_ $ makeCommentsServer <$> logger <*> commentsRepository
  runnerConf <- addIOEff_ $ Bean.JsonConf.lookupSection @RunnerConf "runner" <$> jsonConf
  addVal_ $ makeRunner <$> runnerConf <*> sqlitePool <*> threadLocal <*> logger <*> commentsServer

polymorphicallyWired' :: Managed (Identity Runner)
polymorphicallyWired' = polymorphicallyWired

polymorphicallyWired'' :: Cauldron Managed
polymorphicallyWired'' = execBuilder $ polymorphicallyWired
