{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

module Comments.Cauldron (cauldron) where

import Bean.ThreadLocal
import Bean.Current
import Bean.JsonConf
import Bean.JsonConf.YamlFile qualified
import Bean.Sqlite.Pool
import Cauldron
import Cauldron.Managed
import Comments.Repository
import Comments.Repository.Sqlite qualified
import Comments.Runner
import Comments.Server
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Data.Function ((&))
import Log
import Log.Backend.StandardOutput
import Servant.Server (Handler)
import Sqlite (Connection)

cauldron :: Cauldron Managed
cauldron = do
  let liftConIO = hoistConstructor liftIO
  emptyCauldron
    & do
      let makeJsonConf = Bean.JsonConf.YamlFile.make do Bean.JsonConf.YamlFile.loadYamlSettings ["conf.yaml"] [] Bean.JsonConf.YamlFile.useEnv
      insert @(JsonConf IO) do makeBean do liftConIO do pack effect do makeJsonConf
    & insert @Logger do makeBean do pack effect do managed withStdOutLogger
    & insert @SqlitePoolConf do makeBean do liftConIO do pack effect do Bean.JsonConf.lookupSection @IO "sqlite"
    & insert @SqlitePool do makeBean do pack effect \conf -> managed do Bean.Sqlite.Pool.make conf
    & insert @(ThreadLocal Connection) do makeBean do liftConIO do pack effect do makeThreadLocal
    & insert @(Current Connection) do makeBean do pack value do makeThreadLocalCurrent
    & insert @CommentsRepository do makeBean do pack value do Comments.Repository.Sqlite.make
    & insert @CommentsServer do makeBean do pack value makeCommentsServer
    & insert @RunnerConf do makeBean do liftConIO do pack effect do Bean.JsonConf.lookupSection @IO "runner"
    & insert @Runner do makeBean do pack value makeRunner
