{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings #-}

module Comments.Server (CommentsServer (..), makeCommentsServer) where

import Comments.Api
import Log
import Lucid
import Lucid.Html5
import Servant
import Control.Monad.Trans.Reader

newtype CommentsServer m = CommentsServer {server :: ServerT Api m}

makeCommentsServer :: Logger -> CommentsServer (ReaderT env IO)
makeCommentsServer logger = CommentsServer {server}
  where
    server =
      Comments
        { mainPage = do
            runLogT "main" logger defaultLogLevel do logInfo_ "Hi there"
            pure do div_ "foo"
        }
