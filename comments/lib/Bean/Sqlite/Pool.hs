{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
module Bean.Sqlite.Pool where

import Data.Pool.Introspection
import Sqlite
import Data.Aeson
import GHC.Generics (Generic)

data SqliteConf = SqliteConf {
    sqliteFile :: FilePath,
    poolSize :: Int
}
  deriving stock (Generic)
  deriving anyclass (FromJSON, ToJSON)

type SqlitePool = Pool Connection
