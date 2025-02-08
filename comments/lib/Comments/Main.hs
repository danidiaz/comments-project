{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Comments.Main
  ( appMain,
    manuallyWiredAppMain,
    polymorphicallyWiredAppMain',
    polymorphicallyWiredAppMain'',
  )
where

import Bean.Current
import Bean.JsonConf
import Bean.JsonConf.YamlFile qualified
import Sqlite.Pool
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
     in recipe @JsonConf $ ioEff $ wire makeJsonConf,
    recipe @Logger $ eff $ wire $ managed withStdOutLogger,
    recipe @SqlitePoolConf $ ioEff $ wire $ Bean.JsonConf.lookupSection @SqlitePoolConf "sqlite",
    recipe @SqlitePool $ eff $ wire \conf -> managed $ Sqlite.Pool.make conf,
    recipe @(ThreadLocal Connection) $ ioEff $ wire makeThreadLocal,
    recipe @(Current Connection) $ val $ wire makeThreadLocalCurrent,
    recipe @CommentsRepository $ val $ wire Comments.Repository.Sqlite.make,
    recipe @CommentsServer $ val $ wire makeCommentsServer,
    recipe @RunnerConf $ ioEff $ wire $ Bean.JsonConf.lookupSection @RunnerConf "runner",
    recipe @Runner $ val $ wire $ makeRunner
  ]

manuallyWired :: Managed Runner
manuallyWired = do
  jsonConf <-
    let makeJsonConf = Bean.JsonConf.YamlFile.make $ Bean.JsonConf.YamlFile.loadYamlSettings ["conf.yaml"] [] Bean.JsonConf.YamlFile.useEnv
     in liftIO $ makeJsonConf
  logger <- managed withStdOutLogger
  sqlitePoolConf <- liftIO $ Bean.JsonConf.lookupSection @SqlitePoolConf "sqlite" jsonConf
  sqlitePool <- managed $ Sqlite.Pool.make sqlitePoolConf
  threadLocal <- liftIO makeThreadLocal
  let currentConnection = makeThreadLocalCurrent threadLocal
  let commentsRepository = Comments.Repository.Sqlite.make logger currentConnection
  let commentsServer = makeCommentsServer logger commentsRepository
  runnerConf <- liftIO $ Bean.JsonConf.lookupSection @RunnerConf "runner" jsonConf
  pure $ makeRunner runnerConf sqlitePool threadLocal logger commentsServer

manuallyWiredAppMain :: IO ()
manuallyWiredAppMain = do
  with manuallyWired \Runner {runServer} -> runServer

polymorphicallyWired :: (MonadWiring m, ConstructorMonad m ~ Managed) => m (ArgsApplicative m Runner)
polymorphicallyWired = do
  jsonConf <- do
    let makeJsonConf = Bean.JsonConf.YamlFile.make $ Bean.JsonConf.YamlFile.loadYamlSettings ["conf.yaml"] [] Bean.JsonConf.YamlFile.useEnv
    _ioEff_ $ pure makeJsonConf
  logger <- _eff_ $ pure $ managed withStdOutLogger
  sqlitePoolConf <- _ioEff_ $ Bean.JsonConf.lookupSection @SqlitePoolConf "sqlite" <$> jsonConf
  sqlitePool <- _eff_ $ (\conf -> managed $ Sqlite.Pool.make conf) <$> sqlitePoolConf
  threadLocal <- _ioEff_ $ pure $ makeThreadLocal
  currentConnection <- _val_ $ makeThreadLocalCurrent <$> threadLocal
  commentsRepository <- _val_ $ Comments.Repository.Sqlite.make <$> logger <*> currentConnection
  commentsServer <- _val_ $ makeCommentsServer <$> logger <*> commentsRepository
  runnerConf <- _ioEff_ $ Bean.JsonConf.lookupSection @RunnerConf "runner" <$> jsonConf
  _val_ $ makeRunner <$> runnerConf <*> sqlitePool <*> threadLocal <*> logger <*> commentsServer

polymorphicallyWired' :: Managed Runner
polymorphicallyWired' = runIdentity <$> polymorphicallyWired

polymorphicallyWiredAppMain' :: IO ()
polymorphicallyWiredAppMain' = do
  with polymorphicallyWired' \Runner {runServer} -> runServer

polymorphicallyWired'' :: IO (Cauldron Managed)
polymorphicallyWired'' = polymorphicallyWired & execBuilder & either throwIO pure

polymorphicallyWiredAppMain'' :: IO ()
polymorphicallyWiredAppMain'' = do
  theCauldron <- polymorphicallyWired''
  cook forbidDepCycles theCauldron & either throwIO \action -> with action \beans -> do
    case taste beans of
      Nothing -> error "no bean found"
      Just Runner {runServer} -> runServer
