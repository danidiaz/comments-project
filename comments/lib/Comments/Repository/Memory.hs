module Comments.Repository.Memory where

import Comments
import Comments.Repository
import Data.Text
import Data.Tuple
import Log
import Sqlite.Query
import Data.IORef
import Data.Sequence as Seq
import Data.Monoid

make ::
  IO CommentsRepository
make askConn = do
  ref <- newIORef @(Seq Comment) mempty
  pure $ CommentsRepository
    { storeComment = \c -> do
        atomicModifyIORef' ref \theSeq -> pure $ theSeq <> Seq.singleton c
      listComments = do
        Seq.toList <$> readIORef ref
    }


