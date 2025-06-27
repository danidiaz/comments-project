{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DataKinds #-}
module Comments.Api.WholeServer (StaticServeConf(..), makeApplication_) where

import Network.Wai.Newtypes
import Comments.Api
import Data.Aeson
import Servant.API
import Servant.Server
import Servant.Server.StaticFiles
import GHC.Generics (Generic)
import Data.Proxy

data StaticServeConf = StaticServeConf
  { 
    staticAssetsFolder :: FilePath
  }
  deriving stock (Generic)
  deriving anyclass (FromJSON, ToJSON)

makeApplication_ :: Server Api -> StaticServeConf -> Application_
makeApplication_ server StaticServeConf {staticAssetsFolder} = Application_ {application}
    where
      staticAssetsServer = serveDirectoryWebApp staticAssetsFolder
      application :: Application
      application = serve (Proxy @(Api :<|> "static" :> Raw)) do server :<|> staticAssetsServer
