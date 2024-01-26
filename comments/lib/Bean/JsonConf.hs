module Bean.JsonConf (JsonConf(..), 
    JsonConfMissingSection(..), 
    JsonConfUnparseableSection(..),
    Key) where

import Data.Aeson
import Control.Exception

data JsonConf m = JsonConf {
    lookupSection :: forall conf. FromJSON conf => Key -> m conf
}

data JsonConfMissingSection = JsonConfMissingSection Key deriving Show
instance Exception JsonConfMissingSection

data JsonConfUnparseableSection = JsonConfUnparseableSection Key String deriving Show
instance Exception JsonConfUnparseableSection