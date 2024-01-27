{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NoFieldSelectors #-}

module Comments.Api (Api, Comments (..), IncomingComment (..)) where

import Data.Text (Text)
import GHC.Generics
import GHC.Generics (Generic)
import Lucid
import Servant.API
import Servant.API.NamedRoutes
import Servant.HTML.Lucid
import Web.FormUrlEncoded

type Api = "comments" :> NamedRoutes Comments

data Comments mode = Comments
  { mainPage :: mode :- Get '[HTML] (Html ()),
    addComment ::
      mode
        :- ReqBody '[FormUrlEncoded] IncomingComment
          :> Post '[HTML] (Html ())
  }
  deriving stock (Generic)

data IncomingComment = IncomingComment {commentText :: Text} deriving (Generic)

instance FromForm IncomingComment
