{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoFieldSelectors #-}

module Comments.Server (CommentsServer (..), makeCommentsServer) where

import Comments
import Comments.Api
import Comments.Repository
import Control.Monad.Catch
import Control.Monad.Trans.Reader
import Data.Foldable
import Log
import Lucid
import Lucid.Html5
import Servant

newtype CommentsServer m = CommentsServer {server :: ServerT Api m}

makeCommentsServer ::
  Logger ->
  CommentsRepository (ReaderT env IO) ->
  CommentsServer (ReaderT env IO)
makeCommentsServer logger CommentsRepository {storeComment, listComments} =
  CommentsServer {server}
  where
    server =
      Comments
        { mainPage = do
            runLogT "main" logger defaultLogLevel do logInfo_ "Hi there"
            comments <- listComments
            pure do
              div_ do
                Data.Foldable.for_ comments \Comment {commentText} -> do
                  p_ do toHtml commentText
              form_ [method_ "post"] do
                textarea_ [id_ "comment", name_ "commentText", rows_ "5", cols_ "33"] mempty
                input_ [type_ "submit", value_ "Send"],
          addComment = \IncomingComment {commentText} -> do
            storeComment Comment {commentText}
            throwM err303
            -- pure "foo"
        }
