module Main where

import Cauldron
import Comments.Cauldron (cauldron)
import Comments.Runner (Runner (..))

main :: IO ()
main =
  case cook forbidDepCycles cauldron of
    Left badBeans -> print badBeans
    Right (_, action) -> do
      boiledBeans <- action
      case taste boiledBeans of
        Nothing -> error "no bean found"
        Just Runner {runServer} -> runServer
