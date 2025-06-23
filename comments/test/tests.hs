module Main (main) where

import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Comments tests"
  [ testCase "Sanity check" $
      pure ()
  ]
