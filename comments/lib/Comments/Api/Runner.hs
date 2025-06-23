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
import Control.Monad.IO.Class
import Control.Monad.Trans.Except
import Control.Monad.Trans.Reader
import Data.Aeson
import Data.Coerce
import Data.Pool.Introspection
import Data.Proxy
import GHC.Generics (Generic)
import Log
import Network.Wai.Handler.Warp (run)
import Servant.API
import Servant.Server
import Servant.Server.StaticFiles
import Sqlite
import Sqlite.Pool
import ThreadLocal

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
  SqlitePool ->
  ThreadLocal Connection ->
  Logger ->
  CommentsServer ->
  Runner
makeRunner
  conf@RunnerConf {port, staticAssetsFolder}
  pool
  threadLocalConnection
  logger
  CommentsServer {server} = Runner {runServer}
    where
      withEachRequest :: forall z. Handler z -> Handler z
      withEachRequest (MkHandler action) = MkHandler do
        withResource pool \Resource {resource} ->
          withThreadLocal threadLocalConnection resource action
      hoistedServer =
        hoistServer
          (Proxy @Api)
          withEachRequest
          server
      staticAssetsServer = serveDirectoryWebApp staticAssetsFolder
      app :: Application
      app = serve (Proxy @(Api :<|> "static" :> Raw)) do hoistedServer :<|> staticAssetsServer
      runServer = do
        runLogT "runner" logger defaultLogLevel do logInfo "Runner started" conf
        run port app
