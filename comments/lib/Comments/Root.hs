{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Comments.Root
  ( cauldron,
    appMain,
    manuallyWiredAppMain,
    dependencyGraphMain,
  )
where

import Cauldron
import Cauldron.Managed
import Comments.Api (CommentsLinks, makeLinks)
import Comments.Api.Runner
import Comments.Api.Server
import Comments.Sqlite
import Comments.Repository
import Comments.Repository.Sqlite qualified
import Control.Exception (throwIO)
import Control.Monad.IO.Class
import Data.Function ((&))
import JsonConf
import JsonConf.YamlFile qualified
import Log
import Log.Backend.StandardOutput
import Sqlite (Connection)
import Sqlite.Pool
import ThreadLocal
import Comments.Api.WholeServer 
import Network.Wai.Bean

dependencyGraphMain :: IO ()
dependencyGraphMain = do
  let depGraph = getDependencyGraph cauldron
  writeAsDot (defaultStyle Nothing) "beans.dot" $ depGraph
  writeAsDot (defaultStyle Nothing) "beans-decos.dot" $ collapseBeans $ removeAggregates $ depGraph
  writeAsDot (defaultStyle Nothing) "beans-simple.dot" $ collapseBeans $ removeDecos $ removeAggregates $ depGraph

appMain :: IO ()
appMain = do
  cook @Runner forbidDepCycles cauldron & either throwIO \action ->
    with action \(Runner {runServer}) -> do
      runServer

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
    recipe @CommentsServer $ Recipe {
      bean = val $ wire makeCommentsServer,
      decos = [
        val $ wire $ Comments.Sqlite.hoistWithConnection Comments.Api.Server.hoistCommentsServer 
      ]
    },
    recipe @StaticServeConf $ ioEff $ wire $ JsonConf.lookupSection @StaticServeConf "runner",
    recipe @Application_ $ val $ wire $ \(CommentsServer { server }) -> Comments.Api.WholeServer.makeApplication_ server,
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
  threadLocalConn <- liftIO makeThreadLocal
  let currentConnection = readThreadLocal threadLocalConn
  let commentsRepository = Comments.Repository.Sqlite.make logger currentConnection
  links <- liftIO makeLinks
  let CommentsServer { server = commentsServer } = 
        makeCommentsServer logger links commentsRepository &
        Comments.Sqlite.hoistWithConnection Comments.Api.Server.hoistCommentsServer sqlitePool threadLocalConn
  staticServeConf <- liftIO $ JsonConf.lookupSection @StaticServeConf "runner" jsonConf
  let application_ = makeApplication_ commentsServer staticServeConf
  runnerConf <- liftIO $ JsonConf.lookupSection @RunnerConf "runner" jsonConf
  pure $ makeRunner runnerConf logger application_

manuallyWiredAppMain :: IO ()
manuallyWiredAppMain = do
  with manuallyWired \Runner {runServer} -> runServer
