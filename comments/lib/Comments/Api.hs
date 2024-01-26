{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE DataKinds #-}
module Comments.Api () where

import Servant.API
import Servant.API.NamedRoutes
import GHC.Generics
import Lucid
import Servant.HTML.Lucid

type Api = NamedRoutes Comments

data Comments mode = Comments {
        mainPage :: Get '[HTML] (Html ())
    } deriving stock Generic

