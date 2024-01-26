{-# LANGUAGE BlockArguments #-}

module Comments.Cauldron (cauldron) where

import Cauldron
import Cauldron.Managed
import Comments.Runner
import Comments.Server
import Data.Function ((&))
import Log
import Log.Backend.StandardOutput
import Bean.JsonConf
import Bean.JsonConf.YamlFile qualified
import Control.Monad.IO.Class

cauldron :: Cauldron Managed
cauldron =
  emptyCauldron
    & do
      let makeJsonConf = Bean.JsonConf.YamlFile.make do Bean.JsonConf.YamlFile.loadYamlSettings ["conf.yaml"] [] Bean.JsonConf.YamlFile.useEnv
      insert @(JsonConf IO) do makeBean do hoistConstructor liftIO do pack effect do makeJsonConf
    & insert @Logger do makeBean do pack effect do managed withStdOutLogger
    & insert @CommentsServer do makeBean do pack value makeCommentsServer
    & insert @Runner do makeBean do pack value makeRunner
