module Comments.Repository where

import Comments

data CommentsRepository = CommentsRepository
  { storeComment :: Comment -> IO (),
    listComments :: IO [Comment]
  }
