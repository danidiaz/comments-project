{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}

module Comments.Api.WholeServer (StaticServeConf (..), makeApplication_) where

import Comments.Api
import Data.Aeson
import Data.Proxy
import GHC.Generics (Generic)
import Network.Wai.Bean
import Servant.API
import Servant.Server
import Servant.Server.StaticFiles

data StaticServeConf = StaticServeConf
  { staticAssetsFolder :: FilePath
  }
  deriving stock (Generic)
  deriving anyclass (FromJSON, ToJSON)

makeApplication_ :: Server Api -> StaticServeConf -> Application_
makeApplication_ server StaticServeConf {staticAssetsFolder} = Application_ {application}
  where
    staticAssetsServer = serveDirectoryWebApp staticAssetsFolder
    application :: Application
    application = serve (Proxy @(Api :<|> "static" :> Raw)) do server :<|> staticAssetsServer
