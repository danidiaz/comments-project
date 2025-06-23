{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DataKinds #-}
module Comments.Api.Wai (StaticServeConf(..), makeApplication_) where

import Network.Wai.Newtypes
import Comments.Api
import Comments.Api.Server
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

makeApplication_ :: StaticServeConf -> CommentsServer -> Application_
makeApplication_ StaticServeConf {staticAssetsFolder} CommentsServer {server} = Application_ {application}
    where
      staticAssetsServer = serveDirectoryWebApp staticAssetsFolder
      application :: Application
      application = serve (Proxy @(Api :<|> "static" :> Raw)) do server :<|> staticAssetsServer
