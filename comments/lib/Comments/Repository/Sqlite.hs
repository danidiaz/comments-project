{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
module Comments.Repository.Sqlite where

import Comments
import Comments.Repository
import Bean.Sqlite.CurrentConnection
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Log
import Sqlite.Query
import Data.Tuple
import Data.Text

make :: 
    Logger -> 
    CurrentConnection (ReaderT env IO) -> 
    CommentsRepository (ReaderT env IO)
make logger CurrentConnection {askCurrentConnection} = do
    CommentsRepository {
        storeComment = \c -> pure (),
        listComments = do 
          conn <- askCurrentConnection
          commentTexts :: [Solo Text] <- liftIO do Sqlite.Query.select_ conn "select comment_text from comment;"
          pure do Comment . getSolo <$> commentTexts
    }
    