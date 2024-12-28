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
import Cauldron.Managed
import Comments.Repository
import Comments.Repository.Sqlite qualified
import Comments.Runner
import Comments.Server
import Control.Exception (throwIO)
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Data.Function ((&))
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
     in recipe @JsonConf $ liftConIO $ eff $ wire makeJsonConf,
    recipe @Logger $ eff $ wire $ managed withStdOutLogger,
    recipe @SqlitePoolConf $ liftConIO $ eff $ wire $ Bean.JsonConf.lookupSection @SqlitePoolConf "sqlite",
    recipe @SqlitePool $ eff $ wire \conf -> managed $ Bean.Sqlite.Pool.make conf,
    recipe @(ThreadLocal Connection) $ liftConIO $ eff $ wire makeThreadLocal,
    recipe @(Current Connection) $ val $ wire makeThreadLocalCurrent,
    recipe @CommentsRepository $ val $ wire Comments.Repository.Sqlite.make,
    recipe @CommentsServer $ val $ wire makeCommentsServer,
    recipe @RunnerConf $ liftConIO $ eff $ wire $ Bean.JsonConf.lookupSection @RunnerConf "runner",
    recipe @Runner $ val $ wire $ makeRunner
  ]
  where
    liftConIO = hoistConstructor liftIO

manuallyWired :: Managed Runner
manuallyWired = do
  jsonConfBean <-
    let makeJsonConf = Bean.JsonConf.YamlFile.make $ Bean.JsonConf.YamlFile.loadYamlSettings ["conf.yaml"] [] Bean.JsonConf.YamlFile.useEnv
     in liftIO $ makeJsonConf
  loggerBean <- managed withStdOutLogger
  sqlitePoolConfBean <- liftIO $ Bean.JsonConf.lookupSection @SqlitePoolConf "sqlite" jsonConfBean
  sqlitePoolBean <- managed $ Bean.Sqlite.Pool.make sqlitePoolConfBean
  threadLocalBean <- liftIO makeThreadLocal
  let currentConnectionBean = makeThreadLocalCurrent threadLocalBean
  let commentsRepossitoryBean = Comments.Repository.Sqlite.make loggerBean currentConnectionBean
  let commentsServerBean = makeCommentsServer loggerBean commentsRepossitoryBean
  runnerConfBean <- liftIO $ Bean.JsonConf.lookupSection @RunnerConf "runner" jsonConfBean
  pure $ makeRunner runnerConfBean sqlitePoolBean threadLocalBean loggerBean commentsServerBean

manuallyWiredAppMain :: IO ()
manuallyWiredAppMain = do
  with manuallyWired \Runner {runServer} -> runServer
