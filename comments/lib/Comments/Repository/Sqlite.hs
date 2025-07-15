{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

module Comments.Repository.Sqlite where

import Comments
import Comments.Repository
import Data.Function ((&))
import Data.Text
import Data.Tuple
import Log
import Sqlite.Query

make ::
  Logger ->
  IO Connection ->
  CommentsRepository
make logger askConn =
  CommentsRepository
    { _storeComment = \Comment {commentText} -> do
        logInfo_ "Storing comment." & runLog
        conn <- askConn
        Sqlite.Query.execute
          conn
          "insert into comment (comment_text) values (?)"
          (MkSolo commentText),
      _listComments = do
        logInfo_ "Listing comments." & runLog
        conn <- askConn
        commentTexts :: [Solo Text] <-
          Sqlite.Query.select_
            conn
            "select comment_text from comment"
        pure do Comment . getSolo <$> commentTexts
    }
  where
    runLog = runLogT "sqliterepo" logger defaultLogLevel
