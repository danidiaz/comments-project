{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NumDecimals #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}

module Main (main) where

import Cauldron
import Cauldron.Managed
import Comments.Api
import Comments.Api.Runner (Runner (..))
import Comments.Repository (CommentsRepository)
import Comments.Repository.Memory qualified
import Comments.Root (cauldron)
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async
import Control.Exception
import Data.Function ((&))
import Network.HTTP.Client
import Network.HTTP.Types.Status (statusCode)
import Network.URI
import Test.Tasty
import Test.Tasty.HUnit

targetUri :: CommentsLinks -> URI
targetUri (CommentsLinks links) = links.mainPage

main :: IO ()
main = defaultMain tests

newtype TestBean = TestBean {runTest :: IO ()}

homePageLoads :: CommentsLinks -> Runner -> TestBean
homePageLoads links Runner {runServer} = TestBean do
  race_
    do
      base <-
        parseURI "http://localhost:8000"
          & maybe (assertFailure "malformed uri") pure
      let uri = targetUri links `Network.URI.relativeTo` base
      threadDelay 1e6
      request <- requestFromURI uri
      manager <- newManager defaultManagerSettings
      response <- httpLbs request manager
      let code = statusCode $ responseStatus response
      code & assertEqual "code should be correct" 200
    do
      runServer

tests :: TestTree
tests =
  testGroup
    "Comments tests"
    [ testCase "Home page loads" $ do
        mconcat
          [ cauldron,
            recipe @CommentsRepository $ ioEff_ $ wire Comments.Repository.Memory.make,
            recipe @TestBean $ val_ $ wire homePageLoads
          ]
          & cook @TestBean forbidDepCycles
          & either throwIO \action ->
            with action \(TestBean {runTest}) -> runTest
        pure ()
    ]
