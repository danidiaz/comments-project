{-# LANGUAGE NoFieldSelectors #-}

module JsonConf
  ( JsonConf (..),
    lookupSection,
    JsonConfMissingSection (..),
    JsonConfUnparseableSection (..),
    Key,
  )
where

import Control.Exception
import Data.Aeson

data JsonConf = JsonConf
  { lookupSection_ :: forall conf. (FromJSON conf) => Key -> IO conf
  }

lookupSection :: forall conf. (FromJSON conf) => Key -> JsonConf -> IO conf
lookupSection key (JsonConf {lookupSection_}) = lookupSection_ key

data JsonConfMissingSection = JsonConfMissingSection Key deriving (Show)

instance Exception JsonConfMissingSection

data JsonConfUnparseableSection = JsonConfUnparseableSection Key String deriving (Show)

instance Exception JsonConfUnparseableSection
