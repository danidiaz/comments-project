{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}

module Comments.Repository where

import Comments

data CommentsRepository = CommentsRepository
  { _storeComment :: Comment -> IO (),
    _listComments :: IO [Comment]
  }

storeComment :: Comment -> CommentsRepository -> IO ()
storeComment comment CommentsRepository {_storeComment} = _storeComment comment

listComments :: CommentsRepository -> IO [Comment]
listComments CommentsRepository {_listComments} = _listComments
