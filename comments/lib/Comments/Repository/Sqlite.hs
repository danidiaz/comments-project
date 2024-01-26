{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

module Comments.Repository.Sqlite where

import Bean.Sqlite.CurrentConnection
import Comments
import Comments.Repository
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Data.Text
import Data.Tuple
import Log
import Sqlite.Query

make ::
  Logger ->
  CurrentConnection (ReaderT env IO) ->
  CommentsRepository (ReaderT env IO)
make logger CurrentConnection {askCurrentConnection} = do
  CommentsRepository
    { storeComment = \Comment {commentText} -> do
        conn <- askCurrentConnection
        liftIO do Sqlite.Query.execute conn "insert into comment (comment_text) values (?)" do MkSolo commentText,
      listComments = do
        conn <- askCurrentConnection
        commentTexts :: [Solo Text] <- liftIO do Sqlite.Query.select_ conn "select comment_text from comment"
        pure do Comment . getSolo <$> commentTexts
    }
