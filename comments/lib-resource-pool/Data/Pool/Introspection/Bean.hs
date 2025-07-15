{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NumDecimals #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Data.Pool.Introspection.Bean
  ( PoolConf (..),
    Pool.Pool,
    Pool.Resource (..),
    make,
  )
where

import Control.Exception
import Data.Aeson
import Data.Pool.Introspection qualified as Pool
import Data.Text
import GHC.Generics (Generic)

data PoolConf = PoolConf
  { poolSize :: Int
  }
  deriving stock (Generic)
  deriving anyclass (FromJSON, ToJSON)

make :: forall r. IO r -> (r -> IO ()) -> PoolConf -> forall x. (Pool.Pool r -> IO x) -> IO x
make alloc dealloc PoolConf {poolSize} continuation = do
  let poolConfig = Pool.defaultPoolConfig alloc dealloc 3600 poolSize
  bracket (Pool.newPool poolConfig) Pool.destroyAllResources continuation
