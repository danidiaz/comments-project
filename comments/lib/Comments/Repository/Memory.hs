module Comments.Repository.Memory where

import Comments
import Comments.Repository
import Data.IORef
import Data.Sequence as Seq
import Data.Foldable

make ::
  IO CommentsRepository
make = do
  ref <- newIORef @(Seq Comment) mempty
  pure CommentsRepository
    { storeComment = \c -> do
        atomicModifyIORef' ref $ \theSeq -> (theSeq |> c,()),
      listComments = do
        Data.Foldable.toList <$> readIORef ref
    }


