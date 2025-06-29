{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NumDecimals #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Sqlite.Pool (SqlitePoolConf, SqlitePool, make) where

import Control.Exception
import Data.Aeson
import Data.Pool.Introspection
import Data.Text
import GHC.Generics (Generic)
import Sqlite

data SqlitePoolConf = SqlitePoolConf
  { databaseFile :: Text,
    poolSize :: Int
  }
  deriving stock (Generic)
  deriving anyclass (FromJSON, ToJSON)

type SqlitePool = Pool Connection

make :: forall r. SqlitePoolConf -> (SqlitePool -> IO r) -> IO r
make SqlitePoolConf {databaseFile, poolSize} continuation = do
  -- OpenV2NoMutex is "threaded" mode.
  let openAction = Sqlite.openV2 DefaultVFS [OpenV2ExtendedResultCode, OpenV2NoMutex] OpenV2ReadWrite databaseFile
  let poolConfig = defaultPoolConfig openAction Sqlite.close 3600 poolSize
  bracket (newPool poolConfig) destroyAllResources continuation