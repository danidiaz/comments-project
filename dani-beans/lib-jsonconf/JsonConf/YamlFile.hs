{-# LANGUAGE BlockArguments #-}

module JsonConf.YamlFile
  ( make,
    module Data.Yaml.Config,
  )
where

import JsonConf
import Control.Exception
import Control.Monad.IO.Class
import Data.Aeson (Value)
import Data.Aeson.KeyMap (KeyMap)
import Data.Aeson.KeyMap qualified
import Data.Aeson.Types
import Data.Yaml.Config

make ::
  -- | Usually pass the result of 'Data.Yaml.Config.loadYamlSettings' here.
  IO (KeyMap Value) ->
  IO JsonConf
make action = do
  keyMap :: KeyMap Value <- action
  pure
    JsonConf
      { lookupSection_ = \sectionKey -> do
          case Data.Aeson.KeyMap.lookup sectionKey keyMap of
            Nothing -> throwIO do JsonConfMissingSection sectionKey
            Just foo -> case fromJSON foo of
              Error message -> throwIO do JsonConfUnparseableSection sectionKey message
              Success confSection -> pure confSection
      }
