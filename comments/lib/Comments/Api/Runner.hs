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
import GHC.Generics (Generic)
import Log
import Network.Wai.Newtypes
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
      runServer = do
        runLogT "runner" logger defaultLogLevel do logInfo "Runner started" conf
        run port application
