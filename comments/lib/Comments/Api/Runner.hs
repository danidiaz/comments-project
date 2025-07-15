{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Comments.Api.Runner
  ( RunnerConf (..),
    Runner (..),
    makeRunner,
  )
where

import Data.Aeson
import Data.Function ((&))
import GHC.Generics (Generic)
import Log
import Network.Wai.Bean
import Network.Wai.Handler.Warp (run)

data RunnerConf = RunnerConf
  { port :: Int
  }
  deriving stock (Generic)
  deriving anyclass (FromJSON, ToJSON)

newtype Runner = Runner {runServer :: IO ()}

makeRunner ::
  RunnerConf ->
  Logger ->
  Application_ ->
  Runner
makeRunner
  conf@RunnerConf {port}
  logger
  Application_ {application} = Runner {runServer}
    where
      runLog = runLogT "runner" logger defaultLogLevel
      runServer = do
        logInfo "Runner started" conf & runLog
        run port application
