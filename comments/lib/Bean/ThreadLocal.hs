{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}

module Bean.ThreadLocal (
    ThreadLocal (..), 
    makeThreadLocal, 
    withThreadLocal,
    ThreadLocalValueAlreadyExists (..),
    readThreadLocal,
    ThreadLocalValueMissing (..),
    makeThreadLocalCurrent,
    ) where

-- https://stackoverflow.com/questions/16296571/lazy-vs-strict-implementations-of-data-structures

import Bean.Current
import Control.Concurrent
import Control.Exception
import Data.HashMap.Lazy (HashMap)
import Data.HashMap.Lazy qualified as Map
import Data.IORef
import Data.Tuple (swap)
import Data.Typeable

newtype ThreadLocal v where
  MakeThreadLocal :: IORef (HashMap ThreadId v) -> ThreadLocal v

makeThreadLocal :: IO (ThreadLocal v)
makeThreadLocal = MakeThreadLocal <$> newIORef Map.empty

withThreadLocal :: forall v a. (Typeable v) => ThreadLocal v -> v -> IO a -> IO a
withThreadLocal (MakeThreadLocal ref) v action = do
  threadId <- myThreadId
  bracket_
    do
      u <- atomicModifyIORef' ref \theMap ->
        do
          let alteration = \case
                Nothing -> do
                  ((), Just v)
                Just _ -> do
                  let ex = MakeThreadLocalValueAlreadyExists threadId (typeRep (Proxy @v))
                  (throw ex, Nothing)
          let r = Map.alterF alteration threadId theMap
          swap r
      -- It's important to evaluate the () here to catch possible
      -- exceptions!  I guess using an MVar instead of an IORef would allow
      -- us to throw exceptions using throwIO instead of throw, which is
      -- more precise and understandable. But would the MVar be slower?
      evaluate u
    do atomicModifyIORef' ref \theMap -> (Map.delete threadId theMap, ())
    action

data ThreadLocalValueAlreadyExists where
  MakeThreadLocalValueAlreadyExists :: ThreadId -> TypeRep -> ThreadLocalValueAlreadyExists
  deriving stock (Show)
  deriving anyclass (Exception)

readThreadLocal :: forall v. (Typeable v) => ThreadLocal v -> IO v
readThreadLocal (MakeThreadLocal ref) = do
  threadId <- myThreadId
  theMap <- readIORef ref
  case Map.lookup threadId theMap of
    Nothing ->
      throwIO do MakeThreadLocalValueMissing threadId (typeRep (Proxy @v))
    Just v -> pure v

data ThreadLocalValueMissing where
  MakeThreadLocalValueMissing :: ThreadId -> TypeRep -> ThreadLocalValueMissing
  deriving stock (Show)
  deriving anyclass (Exception)

makeThreadLocalCurrent :: forall v. (Typeable v) => ThreadLocal v -> Current v
makeThreadLocalCurrent theThreadLocal = Current {askCurrent = readThreadLocal theThreadLocal}
