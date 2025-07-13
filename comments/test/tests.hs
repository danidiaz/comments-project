{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NumDecimals #-}
{-# LANGUAGE BlockArguments #-}
module Main (main) where

import Test.Tasty
import Test.Tasty.HUnit
import Comments.Api
import Network.URI
import Network.HTTP.Client
import Network.HTTP.Types.Status (statusCode)
import Control.Concurrent (threadDelay)
import Data.Function ((&))
import Control.Concurrent.Async
import Comments.Api.Runner (Runner(..))
import Comments.Root (cauldron)
import Cauldron
import Cauldron.Managed
import Control.Exception

targetUri :: CommentsLinks -> URI
targetUri (CommentsLinks links) = links.mainPage

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Comments tests"
  [ testCase "Sanity check" $ do
      cauldron 
        & cook @Runner forbidDepCycles 
        & either throwIO \action ->
            with action \(Runner {runServer}) ->
                race_ 
                   do
                     links <- makeLinks
                     let Just base = parseURI "http://localhost:8000"
                     let uri = targetUri links `Network.URI.relativeTo` base
                     threadDelay 1e6
                     request <- requestFromURI uri 
                     manager <- newManager defaultManagerSettings
                     response <- httpLbs request manager
                     let code = statusCode $ responseStatus response
                     code & assertEqual "code should be correct" 200
                   do 
                     runServer
      pure ()
  ]
