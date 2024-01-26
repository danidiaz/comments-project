{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ViewPatterns #-}

module Comments.Runner where

import Comments.Api
import Comments.Server
import Control.Monad.IO.Class
import Data.Proxy
import Network.Wai.Handler.Warp (run)
import Servant.Server

newtype Runner = Runner {runServer :: IO ()}

makeRunner :: CommentsServer -> Runner
makeRunner CommentsServer {server} = Runner {runServer}
  where
    hoistedServer =
      hoistServer
        (Proxy @Api)
        liftIO
        server
    app :: Application
    app = serve (Proxy @Api) hoistedServer
    runServer =
      run 8080 app
