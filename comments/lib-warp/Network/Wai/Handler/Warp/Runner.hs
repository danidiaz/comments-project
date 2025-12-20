{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}

module Network.Wai.Handler.Warp.Runner
  ( RunnerConf (..),
    Runner (..),
    makeRunner,
    decorate,
    run,
  )
where

import Data.Aeson
import GHC.Generics (Generic)
import Network.Wai.Bean
import Network.Wai.Handler.Warp qualified

data RunnerConf = RunnerConf
  { port :: Int
  }
  deriving stock (Generic)
  deriving anyclass (FromJSON, ToJSON)

newtype Runner = Runner {_run :: IO ()}

makeRunner ::
  RunnerConf ->
  Application ->
  Runner
makeRunner
  RunnerConf {port}
  Application {application} = Runner {_run}
    where
      _run = Network.Wai.Handler.Warp.run port application

run :: Runner -> IO ()
run Runner {_run} = _run

decorate :: (forall x. IO x -> IO x) -> Runner -> Runner
decorate f Runner {_run} = Runner {_run = f _run}
