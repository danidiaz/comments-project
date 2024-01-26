module Comments.Repository where

import Comments

data CommentsRepository m = CommentsRepository {
    storeComment :: Comment -> m (),
    listComments :: m [Comment]
}