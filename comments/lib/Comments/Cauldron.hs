{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Comments.Cauldron (cauldron) where

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
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Data.Function ((&))
import Log
import Log.Backend.StandardOutput
import Servant.Server (Handler)
import Sqlite (Connection)

cauldron :: Cauldron Managed
cauldron =
  [ let makeJsonConf = Bean.JsonConf.YamlFile.make do Bean.JsonConf.YamlFile.loadYamlSettings ["conf.yaml"] [] Bean.JsonConf.YamlFile.useEnv
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
