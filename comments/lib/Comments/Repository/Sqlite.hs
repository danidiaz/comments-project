{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

module Comments.Repository.Sqlite where

import Bean.Current
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
  Current Connection ->
  CommentsRepository
make logger Current { askCurrent } = do
  CommentsRepository
    { storeComment = \Comment {commentText} -> do
        conn <- askCurrent
        Sqlite.Query.execute conn "insert into comment (comment_text) values (?)" do MkSolo commentText,
      listComments = do
        conn <- askCurrent
        commentTexts :: [Solo Text] <- Sqlite.Query.select_ conn "select comment_text from comment"
        pure do Comment . getSolo <$> commentTexts
    }
