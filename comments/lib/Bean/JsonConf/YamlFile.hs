{-# LANGUAGE BlockArguments #-}
module Bean.JsonConf.YamlFile (
        make,
        module Data.Yaml.Config
    ) where

import Bean.JsonConf
import Control.Monad.IO.Class
import Data.Yaml.Config
import Data.Aeson ( Value )
import Data.Aeson.Types 
import Data.Aeson.KeyMap ( KeyMap )
import Data.Aeson.KeyMap qualified
import Control.Monad.Catch (MonadThrow, throwM)

make :: MonadThrow m => 
    -- | Usually pass the result of 'Data.Yaml.Config.loadYamlSettings' here.
    IO (KeyMap Value) ->
    IO (JsonConf m)
make action = do
    keyMap :: KeyMap Value <- liftIO action
    pure JsonConf { 
        lookupSection = \sectionKey -> do
            case Data.Aeson.KeyMap.lookup sectionKey keyMap of
                Nothing -> throwM do JsonConfMissingSection sectionKey
                Just foo -> case fromJSON foo of 
                    Error message -> throwM do JsonConfUnparseableSection sectionKey message
                    Success confSection -> pure confSection
    }



