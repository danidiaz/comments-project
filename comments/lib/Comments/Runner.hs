{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ViewPatterns #-}

module Comments.Runner (RunnerConf (..), Runner (..), makeRunner) where

import Comments.Api
import Comments.Server
import Control.Monad.IO.Class
import Data.Aeson
import Data.Proxy
import GHC.Generics (Generic)
import Network.Wai.Handler.Warp (run)
import Servant.Server

newtype RunnerConf = RunnerConf
  { port :: Int
  }
  deriving stock (Generic)
  deriving anyclass (FromJSON)

newtype Runner = Runner {runServer :: IO ()}

makeRunner :: RunnerConf -> CommentsServer -> Runner
makeRunner RunnerConf {port} CommentsServer {server} = Runner {runServer}
  where
    hoistedServer =
      hoistServer
        (Proxy @Api)
        liftIO
        server
    app :: Application
    app = serve (Proxy @Api) hoistedServer
    runServer =
      run port app
