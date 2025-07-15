{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}

-- | Allocating connections that might me used down the hierarchy of calls.
module Comments.Sqlite
  ( withConnection,
    hoistWithConnection,
    SqlitePoolConf (..),
    SqlitePool,
    makeSqlitePool,
  )
where

import Data.Aeson
import Data.Pool.Introspection qualified
import Data.Pool.Introspection.Bean
import Data.Text
import GHC.Generics (Generic)
import Sqlite
import ThreadLocal

type SqlitePool = Pool Connection

data SqlitePoolConf = SqlitePoolConf
  {databaseFile :: Text}
  deriving stock (Generic)
  deriving anyclass (FromJSON, ToJSON)

makeSqlitePool :: SqlitePoolConf -> PoolConf -> forall x. (SqlitePool -> IO x) -> IO x
makeSqlitePool
  SqlitePoolConf {databaseFile}
  poolConf =
    do
      Data.Pool.Introspection.Bean.make (Sqlite.openV2NoMutexReadWrite databaseFile) Sqlite.close poolConf

withConnection ::
  SqlitePool ->
  ThreadLocal Connection ->
  (forall x. IO x -> IO x)
withConnection pool threadLocalConnection action =
  Data.Pool.Introspection.withResource pool \Resource {resource} ->
    withThreadLocal threadLocalConnection resource action

-- | Like 'withConnection', but more in the shape of a decorator.
hoistWithConnection ::
  forall bean.
  ((forall x. IO x -> IO x) -> bean -> bean) ->
  SqlitePool ->
  ThreadLocal Connection ->
  bean ->
  bean
hoistWithConnection hoistBean pool tlocal =
  hoistBean (withConnection pool tlocal)
