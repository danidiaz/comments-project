module Comments.Repository where

import Comments.Model

data CommentsRepository = CommentsRepository
  { storeComment :: Comment -> IO (),
    listComments :: IO [Comment]
  }
