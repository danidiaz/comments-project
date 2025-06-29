{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass #-}
-- | Allocating connections that might me used down the hierarchy of calls.
module Comments.Sqlite (
        withConnection,
        hoistWithConnection,
        SqlitePoolConf(..),
        SqlitePool,
        makeSqlitePool,
    ) where

import Sqlite
import Data.Pool.Introspection qualified
import Data.Pool.Introspection.Bean
import ThreadLocal
import Data.Text
import GHC.Generics (Generic)
import Data.Aeson

type SqlitePool = Pool Connection

data SqlitePoolConf = SqlitePoolConf
  { databaseFile :: Text }
  deriving stock (Generic)
  deriving anyclass (FromJSON, ToJSON)

makeSqlitePool :: SqlitePoolConf -> PoolConf -> forall x. (SqlitePool -> IO x) -> IO x
makeSqlitePool SqlitePoolConf {databaseFile} 
     poolConf
     = do
        Data.Pool.Introspection.Bean.make (Sqlite.openV2NoMutexReadWrite databaseFile) Sqlite.close poolConf

-- 
-- type SqlitePool = Pool.Pool Connection
-- 
-- make :: forall r. IO r -> (r -> IO ()) -> PoolConf -> forall x. (Pool.Pool r -> IO x) -> IO x
-- make SqlitePoolConf {databaseFile, poolSize} continuation = do
--   let poolConfig = Pool.defaultPoolConfig (Sqlite.openV2NoMutexReadWrite databaseFile) Sqlite.close 3600 poolSize
--   bracket (Pool.newPool poolConfig) Pool.destroyAllResources continuation

withConnection :: 
  SqlitePool ->
  ThreadLocal Connection -> (forall x . IO x -> IO x)
withConnection pool threadLocalConnection action =
    Data.Pool.Introspection.withResource pool \Resource {resource} ->
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
