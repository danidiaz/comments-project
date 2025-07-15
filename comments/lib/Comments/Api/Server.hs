{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoFieldSelectors #-}

module Comments.Api.Server
  ( CommentsServer (..),
    unwrap,
    makeCommentsServer,
    hoistCommentsServer,
  )
where

import Comments
import Comments.Api
import Comments.Repository
import Control.Monad.Trans.Except
import Data.Coerce
import Data.Foldable
import Log
import Lucid
-- import Lucid.Servant
import Network.HTTP.Types.Header
import Network.URI (uriToString)
import Servant

newtype CommentsServer = CommentsServer {server :: Server Api}

unwrap :: CommentsServer -> Server Api
unwrap CommentsServer {server} = server

makeCommentsServer ::
  Logger ->
  CommentsLinks ->
  CommentsRepository ->
  CommentsServer
makeCommentsServer logger CommentsLinks {links} CommentsRepository {storeComment, listComments} =
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
          addComment = \IncomingComment {commentText} -> MkHandler do
            storeComment Comment {commentText}
            -- \| https://hachyderm.io/@DiazCarrete/111841132226571708
            pure do
              Left
                err303
                  { errHeaders =
                      [ uriToLocationHeader links.mainPage
                      ]
                  }
        }
    uriToLocationHeader uri =
      (hLocation, toHeader $ uriToString id uri "")

handlerize :: IO r -> Handler r
handlerize action = coerce do fmap (Right @ServerError) action

hoistCommentsServer :: (forall x. IO x -> IO x) -> CommentsServer -> CommentsServer
hoistCommentsServer f CommentsServer {server = _server} = CommentsServer {server}
  where
    server = hoistServer (Proxy @Api) h _server
    h (MkHandler action) = MkHandler (f action)
