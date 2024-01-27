{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoFieldSelectors #-}

module Comments.Server (CommentsServer (..), makeCommentsServer) where

import Comments
import Comments.Api
import Comments.Repository
import Control.Monad.Catch ( MonadThrow(throwM) )
import Control.Monad.Trans.Reader
import Data.Foldable
import Log
import Lucid
import Lucid.Html5
import Servant
import Servant.Server (Handler)
import Data.Coerce
import Control.Monad.Trans.Except

newtype CommentsServer m = CommentsServer {server :: ServerT Api m}

makeCommentsServer ::
  Logger ->
  CommentsRepository (ReaderT env IO) ->
  CommentsServer (ReaderT env Handler)
makeCommentsServer logger CommentsRepository {storeComment, listComments} =
  CommentsServer {server}
  where
    server =
      Comments
        { mainPage = handlerize do
            runLogT "main" logger defaultLogLevel do logInfo_ "Hi there"
            comments <- listComments
            pure do
                div_ do
                  Data.Foldable.for_ comments \Comment {commentText} -> do
                    p_ do toHtml commentText
                form_ [method_ "post"] do
                  textarea_ [id_ "comment", name_ "commentText", rows_ "5", cols_ "33"] mempty
                  input_ [type_ "submit", value_ "Send"],
          addComment = \IncomingComment {commentText} -> handlerizeE do
            storeComment Comment {commentText}
            pure do Left err303
        }

handlerize :: ReaderT env IO r -> ReaderT env Handler r
handlerize action = coerce do fmap (Right @ServerError) action

handlerizeE :: ReaderT env IO (Either ServerError r) -> ReaderT env Handler r
handlerizeE = coerce 