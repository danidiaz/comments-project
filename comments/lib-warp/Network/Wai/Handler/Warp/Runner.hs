{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
module Network.Wai.Handler.Warp.Runner
  ( RunnerConf (..),
    Runner (..),
    makeRunner,
    hoistRunner,
    runApplication
  )
where

import Data.Aeson
import Data.Function ((&))
import GHC.Generics (Generic)
import Network.Wai.Bean
import Network.Wai.Handler.Warp (run)

data RunnerConf = RunnerConf
  { port :: Int
  }
  deriving stock (Generic)
  deriving anyclass (FromJSON, ToJSON)

newtype Runner = Runner {_run :: IO ()}

makeRunner ::
  RunnerConf ->
  Application_ ->
  Runner
makeRunner
  RunnerConf {port}
  Application_ {application} = Runner {_run}
    where
      _run = run port application

runApplication :: Runner -> IO ()
runApplication Runner { _run } = _run  

hoistRunner :: (forall x. IO x -> IO x) -> Runner -> Runner
hoistRunner f Runner {_run} =  Runner { _run = f _run }