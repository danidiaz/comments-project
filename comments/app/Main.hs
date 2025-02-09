{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
module Main where

import JsonConf
import JsonConf.YamlFile qualified
import Sqlite.Pool
import ThreadLocal
import Cauldron
import Cauldron.Builder
import Cauldron.Managed
import Comments.Repository
import Comments.Repository.Sqlite qualified
import Comments.Runner
import Comments.Server
import Comments.Api (CommentsLinks, makeLinks)
import Control.Exception (throwIO)
import Control.Monad.IO.Class
import Data.Function ((&))
import Data.Functor.Identity
import Log
import Log.Backend.StandardOutput
import Sqlite (Connection)

main :: IO ()
main = appMain

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
  [ let makeJsonConf = JsonConf.YamlFile.make $ JsonConf.YamlFile.loadYamlSettings ["conf.yaml"] [] JsonConf.YamlFile.useEnv
     in recipe @JsonConf $ ioEff $ wire makeJsonConf,
    recipe @Logger $ eff $ wire $ managed withStdOutLogger,
    recipe @SqlitePoolConf $ ioEff $ wire $ JsonConf.lookupSection @SqlitePoolConf "sqlite",
    recipe @SqlitePool $ eff $ wire \conf -> managed $ Sqlite.Pool.make conf,
    recipe @(ThreadLocal Connection) $ ioEff $ wire makeThreadLocal,
    recipe @(IO Connection) $ val $ wire (readThreadLocal @Connection),
    recipe @CommentsRepository $ val $ wire Comments.Repository.Sqlite.make,
    recipe @CommentsLinks $ ioEff $ wire $ makeLinks, 
    recipe @CommentsServer $ val $ wire makeCommentsServer,
    recipe @RunnerConf $ ioEff $ wire $ JsonConf.lookupSection @RunnerConf "runner",
    recipe @Runner $ val $ wire $ makeRunner
  ]

manuallyWired :: Managed Runner
manuallyWired = do
  jsonConf <-
    let makeJsonConf = JsonConf.YamlFile.make $ JsonConf.YamlFile.loadYamlSettings ["conf.yaml"] [] JsonConf.YamlFile.useEnv
     in liftIO $ makeJsonConf
  logger <- managed withStdOutLogger
  sqlitePoolConf <- liftIO $ JsonConf.lookupSection @SqlitePoolConf "sqlite" jsonConf
  sqlitePool <- managed $ Sqlite.Pool.make sqlitePoolConf
  threadLocal <- liftIO makeThreadLocal
  let currentConnection = readThreadLocal threadLocal
  let commentsRepository = Comments.Repository.Sqlite.make logger currentConnection
  links <- liftIO makeLinks
  let commentsServer = makeCommentsServer logger links commentsRepository
  runnerConf <- liftIO $ JsonConf.lookupSection @RunnerConf "runner" jsonConf
  pure $ makeRunner runnerConf sqlitePool threadLocal logger commentsServer

manuallyWiredAppMain :: IO ()
manuallyWiredAppMain = do
  with manuallyWired \Runner {runServer} -> runServer

polymorphicallyWired :: (MonadWiring m, ConstructorMonad m ~ Managed) => m (ArgsApplicative m Runner)
polymorphicallyWired = do
  jsonConf <- do
    let makeJsonConf = JsonConf.YamlFile.make $ JsonConf.YamlFile.loadYamlSettings ["conf.yaml"] [] JsonConf.YamlFile.useEnv
    _ioEff_ $ pure makeJsonConf
  logger <- _eff_ $ pure $ managed withStdOutLogger
  sqlitePoolConf <- _ioEff_ $ JsonConf.lookupSection @SqlitePoolConf "sqlite" <$> jsonConf
  sqlitePool <- _eff_ $ (\conf -> managed $ Sqlite.Pool.make conf) <$> sqlitePoolConf
  threadLocal <- _ioEff_ $ pure $ makeThreadLocal
  currentConnection <- _val_ $ readThreadLocal <$> threadLocal
  commentsRepository <- _val_ $ Comments.Repository.Sqlite.make <$> logger <*> currentConnection
  links <- _ioEff_ $ pure makeLinks
  commentsServer <- _val_ $ makeCommentsServer <$> logger <*> links <*> commentsRepository
  runnerConf <- _ioEff_ $ JsonConf.lookupSection @RunnerConf "runner" <$> jsonConf
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
