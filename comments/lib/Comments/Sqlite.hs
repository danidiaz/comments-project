{-# LANGUAGE BlockArguments #-}
-- | Allocating connections that might me used down the hierarchy of calls.
module Comments.Sqlite (
        withConnection,
        hoistWithConnection,
    ) where

import Sqlite
import Sqlite.Pool
import Data.Pool.Introspection
import ThreadLocal

withConnection :: 
  SqlitePool ->
  ThreadLocal Connection -> (forall x . IO x -> IO x)
withConnection pool threadLocalConnection action =
    withResource pool \Resource {resource} ->
        withThreadLocal threadLocalConnection resource action

-- | Like 'withConnection', but more in the shape of a decorator.
hoistWithConnection :: forall bean . 
    ((forall x . IO x -> IO x) -> bean -> bean) ->
    SqlitePool ->
    ThreadLocal Connection ->
    bean -> 
    bean
hoistWithConnection hoistBean pool tlocal = 
    hoistBean (withConnection pool tlocal)
