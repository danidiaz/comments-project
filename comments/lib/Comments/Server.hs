{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoFieldSelectors #-}

module Comments.Server (CommentsServer (..), makeCommentsServer) where

import Comments
import Comments.Api
import Comments.Repository
import Control.Monad.Trans.Except
import Data.Coerce
import Data.Foldable
import Log
import Lucid
import Lucid.Html5
-- import Lucid.Servant
import Network.HTTP.Types.Header
import Network.URI (uriToString)
import Servant
import Servant.API
import Servant.Server (Handler)

newtype CommentsServer = CommentsServer {server :: Server Api}

makeCommentsServer ::
  Logger ->
  Links -> 
  CommentsRepository ->
  CommentsServer
makeCommentsServer logger links CommentsRepository {storeComment, listComments} =
  CommentsServer {server}
  where
    server =
      Comments
        { mainPage = handlerize do
            runLogT "main" logger defaultLogLevel do logInfo_ "Hi there"
            comments <- listComments
            pure do
              doctypehtml_ do
                head_ do
                  title_ "comments"
                  link_ [rel_ "stylesheet", type_ "text/css", href_ "/static/styles.css"]
                body_ do
                  main_ do
                    div_ do
                      Data.Foldable.for_ comments \Comment {commentText} -> do
                        p_ do toHtml commentText
                    form_ [method_ "post"] do
                      div_ do textarea_ [id_ "comment", name_ "commentText", rows_ "5"] mempty
                      input_ [type_ "submit", value_ "Send"],
          addComment = \IncomingComment {commentText} -> handlerizeE do
            storeComment Comment {commentText}
            -- \| https://hachyderm.io/@DiazCarrete/111841132226571708
            pure do Left err303 {errHeaders = [
              uriToLocationHeader links.mainPage
              ]
              }
        }
    uriToLocationHeader uri = 
      (hLocation, toHeader $ uriToString id uri "" )

handlerize :: IO r -> Handler r
handlerize action = coerce do fmap (Right @ServerError) action

handlerizeE :: IO (Either ServerError r) -> Handler r
handlerizeE = coerce
