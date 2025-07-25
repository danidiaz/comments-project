module Comments.Repository.Memory where

import Comments
import Comments.Repository
import Data.Foldable
import Data.IORef
import Data.Sequence as Seq

make ::
  IO CommentsRepository
make = do
  ref <- newIORef @(Seq Comment) mempty
  pure
    CommentsRepository
      { _storeComment = \c -> do
          atomicModifyIORef' ref $ \theSeq -> (theSeq |> c, ()),
        _listComments = do
          Data.Foldable.toList <$> readIORef ref
      }
