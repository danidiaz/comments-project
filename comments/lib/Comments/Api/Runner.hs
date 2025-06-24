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

import Comments.Api
import Comments.Api.Server
import Data.Aeson
import Data.Proxy
import GHC.Generics (Generic)
import Log
import Network.Wai.Handler.Warp (run)
import Servant.API
import Servant.Server
import Servant.Server.StaticFiles

data RunnerConf = RunnerConf
  { port :: Int,
    -- Perhaps the static file server could be its own bean with its own
    -- configuration, but let's not overcomplicate things.
    staticAssetsFolder :: FilePath
  }
  deriving stock (Generic)
  deriving anyclass (FromJSON, ToJSON)

newtype Runner = Runner {runServer :: IO ()}

makeRunner ::
  RunnerConf ->
  Logger ->
  CommentsServer ->
  Runner
makeRunner
  conf@RunnerConf {port, staticAssetsFolder}
  logger
  CommentsServer {server} = Runner {runServer}
    where
      staticAssetsServer = serveDirectoryWebApp staticAssetsFolder
      app :: Application
      app = serve (Proxy @(Api :<|> "static" :> Raw)) do server :<|> staticAssetsServer
      runServer = do
        runLogT "runner" logger defaultLogLevel do logInfo "Runner started" conf
        run port app
