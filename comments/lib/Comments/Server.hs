{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings #-}

module Comments.Server (CommentsServer (..), makeCommentsServer) where

import Comments.Api
import Lucid
import Lucid.Html5
import Servant

newtype CommentsServer = CommentsServer {server :: ServerT Api IO}

makeCommentsServer :: CommentsServer
makeCommentsServer = CommentsServer {server}
  where
    server =
      Comments
        { mainPage = do
            pure do div_ "foo"
        }
