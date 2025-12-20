{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}

module Comments.Api.WholeServer (StaticServeConf (..), makeApplication) where

import Comments.Api
import Data.Aeson
import Data.Proxy
import GHC.Generics (Generic)
import Network.Wai.Bean qualified
import Servant.API
import Servant.Server
import Servant.Server.StaticFiles

data StaticServeConf = StaticServeConf
  { staticAssetsFolder :: FilePath
  }
  deriving stock (Generic)
  deriving anyclass (FromJSON, ToJSON)

makeApplication :: Server Api -> StaticServeConf -> Network.Wai.Bean.Application
makeApplication server StaticServeConf {staticAssetsFolder} = Network.Wai.Bean.Application {Network.Wai.Bean.application}
  where
    staticAssetsServer = serveDirectoryWebApp staticAssetsFolder
    application :: Application
    application = serve (Proxy @(Api :<|> "static" :> Raw)) do server :<|> staticAssetsServer
