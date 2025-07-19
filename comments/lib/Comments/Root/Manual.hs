{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Comments.Root.Manual (main) where

import Cauldron.Managed
import Comments.Api (makeLinks)
import Comments.Api.Runner
import Comments.Api.Server
import Comments.Api.WholeServer
import Comments.Repository.Sqlite qualified
import Comments.Sqlite
import Control.Monad.IO.Class
import Data.Function ((&))
import Data.Pool.Introspection.Bean (PoolConf)
import JsonConf
import JsonConf.YamlFile qualified
import Log.Backend.StandardOutput
import ThreadLocal

manuallyWired :: Managed Runner
manuallyWired = do
  jsonConf <-
    let makeJsonConf = JsonConf.YamlFile.make $ JsonConf.YamlFile.loadYamlSettings ["conf.yaml"] [] JsonConf.YamlFile.useEnv
     in liftIO $ makeJsonConf
  logger <- managed withStdOutLogger
  sqlitePoolConf <- liftIO $ JsonConf.lookupSection @SqlitePoolConf "sqlite" jsonConf
  poolConf <- liftIO $ JsonConf.lookupSection @PoolConf "sqlite" jsonConf
  sqlitePool <- managed $ Comments.Sqlite.makeSqlitePool sqlitePoolConf poolConf
  threadLocalConn <- liftIO makeThreadLocal
  let currentConnection = readThreadLocal threadLocalConn
  let commentsRepository = Comments.Repository.Sqlite.make logger currentConnection
  links <- liftIO makeLinks
  let CommentsServer {server = commentsServer} =
        makeCommentsServer logger links commentsRepository
          & Comments.Sqlite.hoistWithConnection Comments.Api.Server.hoistCommentsServer sqlitePool threadLocalConn
  staticServeConf <- liftIO $ JsonConf.lookupSection @StaticServeConf "runner" jsonConf
  let application_ = makeApplication_ commentsServer staticServeConf
  runnerConf <- liftIO $ JsonConf.lookupSection @RunnerConf "runner" jsonConf
  pure $ makeRunner runnerConf application_

main :: IO ()
main = do
  with manuallyWired runApplication
