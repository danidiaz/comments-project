{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NoFieldSelectors #-}

module Comments.Api (Api, Comments (..)) where

import GHC.Generics
import Lucid
import Servant.API
import Servant.API.NamedRoutes
import Servant.HTML.Lucid

type Api = NamedRoutes Comments

data Comments mode = Comments
  { mainPage :: mode :- Get '[HTML] (Html ())
  }
  deriving stock (Generic)
