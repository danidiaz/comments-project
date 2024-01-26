{-# LANGUAGE NoFieldSelectors #-}

module Bean.JsonConf
  ( JsonConf (..),
    lookupSection,
    JsonConfMissingSection (..),
    JsonConfUnparseableSection (..),
    Key,
  )
where

import Control.Exception
import Data.Aeson

data JsonConf m = JsonConf
  { lookupSection_ :: forall conf. (FromJSON conf) => Key -> m conf
  }

lookupSection :: forall m conf. (FromJSON conf) => Key -> JsonConf m -> m conf
lookupSection key (JsonConf {lookupSection_}) = lookupSection_ key

data JsonConfMissingSection = JsonConfMissingSection Key deriving (Show)

instance Exception JsonConfMissingSection

data JsonConfUnparseableSection = JsonConfUnparseableSection Key String deriving (Show)

instance Exception JsonConfUnparseableSection
