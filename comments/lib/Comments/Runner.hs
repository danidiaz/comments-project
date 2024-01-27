{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Comments.Runner
  ( RunnerConf (..),
    Runner (..),
    makeRunner,
  )
where

import Bean.Sqlite.Pool
import Comments.Api
import Comments.Server
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Data.Aeson
import Data.Pool.Introspection
import Data.Proxy
import GHC.Generics (Generic)
import Log
import Network.Wai.Handler.Warp (run)
import Servant.Server
import Sqlite
import Control.Monad.Trans.Except
import Data.Coerce

newtype RunnerConf = RunnerConf
  { port :: Int
  }
  deriving stock (Generic)
  deriving anyclass (FromJSON, ToJSON)

newtype Runner = Runner {runServer :: IO ()}

makeRunner ::
  RunnerConf ->
  SqlitePool ->
  Logger ->
  CommentsServer (ReaderT Connection Handler) ->
  Runner
makeRunner conf@RunnerConf {port} pool logger CommentsServer {server} = Runner {runServer}
  where
    withEachRequest action =
      Handler do ExceptT do withResource pool \Resource {resource} -> runHandler do runReaderT action resource
    hoistedServer =
      hoistServer
        (Proxy @Api)
        withEachRequest
        server
    app :: Application
    app = serve (Proxy @Api) hoistedServer
    runServer = do
      runLogT "runner" logger defaultLogLevel do logInfo "Runner started" conf
      run port app
