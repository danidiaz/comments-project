{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings #-}

module Comments.Server (CommentsServer (..), makeCommentsServer) where

import Comments.Api
import Log
import Lucid
import Lucid.Html5
import Servant

newtype CommentsServer = CommentsServer {server :: ServerT Api IO}

makeCommentsServer :: Logger -> CommentsServer
makeCommentsServer logger = CommentsServer {server}
  where
    server =
      Comments
        { mainPage = do
            runLogT "main" logger defaultLogLevel do logInfo_ "Hi there"
            pure do div_ "foo"
        }
