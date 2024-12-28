{-# LANGUAGE BlockArguments #-}

module Main where

import Cauldron
import Cauldron.Managed
import Comments.Cauldron (cauldron)
import Comments.Runner (Runner (..))
import Control.Exception (throwIO)
import Data.Function ((&))

main :: IO ()
main = do
  let depGraph = getDependencyGraph cauldron
  writeAsDot (defaultStyle Nothing) "beans.dot" $ collapseToPrimaryBeans $ removeDecos $ removeSecondaryBeans $ depGraph
  cook forbidDepCycles cauldron & either throwIO \action -> with action \beans -> do
    case taste beans of
      Nothing -> error "no bean found"
      Just Runner {runServer} -> runServer
