{-# LANGUAGE BlockArguments #-}

module Main where

import Cauldron
import Cauldron.Managed
import Comments.Cauldron (cauldron)
import Comments.Runner (Runner (..))

main :: IO ()
main =
  case cook forbidDepCycles cauldron of
    Left badBeans -> print badBeans
    Right (depGraph, action) -> do
      exportToDot "beans.dot" do collapsePrimaryBeans do removeDecos do removeSecondaryBeans do depGraph
      with action \boiledBeans -> do
        case taste boiledBeans of
          Nothing -> error "no bean found"
          Just Runner {runServer} -> runServer
