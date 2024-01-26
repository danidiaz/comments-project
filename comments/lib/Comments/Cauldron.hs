{-# LANGUAGE BlockArguments #-}

module Comments.Cauldron (cauldron) where

import Cauldron
import Cauldron.Managed
import Comments.Runner
import Comments.Server
import Data.Function ((&))
import Log
import Log.Backend.StandardOutput

cauldron :: Cauldron Managed
cauldron =
  emptyCauldron
    & insert @Logger do makeBean do pack effect do managed withStdOutLogger
    & insert @CommentsServer do makeBean do pack value makeCommentsServer
    & insert @Runner do makeBean do pack value makeRunner
