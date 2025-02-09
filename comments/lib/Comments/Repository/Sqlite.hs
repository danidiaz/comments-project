{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

module Comments.Repository.Sqlite where

import Comments
import Comments.Repository
import Control.Monad.IO.Class
import Data.Text
import Data.Tuple
import Log
import Sqlite.Query

make ::
  Logger ->
  IO Connection ->
  CommentsRepository
make logger askConn = do
  CommentsRepository
    { storeComment = \Comment {commentText} -> do
        conn <- askConn
        Sqlite.Query.execute conn "insert into comment (comment_text) values (?)" do MkSolo commentText,
      listComments = do
        conn <- askConn
        commentTexts :: [Solo Text] <- Sqlite.Query.select_ conn "select comment_text from comment"
        pure do Comment . getSolo <$> commentTexts
    }
