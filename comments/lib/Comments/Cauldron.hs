{-# LANGUAGE BlockArguments #-}

module Comments.Cauldron (cauldron) where

import Cauldron
import Comments.Runner
import Comments.Server
import Data.Function ((&))

cauldron :: Cauldron IO
cauldron =
  emptyCauldron
    & insert @CommentsServer do makeBean do pack value makeCommentsServer
    & insert @Runner do makeBean do pack value makeRunner
