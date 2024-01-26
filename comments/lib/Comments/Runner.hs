{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Comments.Runner (RunnerConf (..), 
  Runner (..), makeRunner) where

import Comments.Api
import Comments.Server
import Control.Monad.IO.Class
import Data.Aeson
import Data.Proxy
import GHC.Generics (Generic)
import Network.Wai.Handler.Warp (run)
import Servant.Server
import Log

newtype RunnerConf = RunnerConf
  { port :: Int
  }
  deriving stock (Generic)
  deriving anyclass (FromJSON, ToJSON)

newtype Runner = Runner {runServer :: IO ()}

makeRunner :: Logger -> RunnerConf -> CommentsServer -> Runner
makeRunner logger conf@RunnerConf {port} CommentsServer {server} = Runner {runServer}
  where
    hoistedServer =
      hoistServer
        (Proxy @Api)
        liftIO
        server
    app :: Application
    app = serve (Proxy @Api) hoistedServer
    runServer = do
      runLogT "runner" logger defaultLogLevel do logInfo "Runner started" conf
      run port app
