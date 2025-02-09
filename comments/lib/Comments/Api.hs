{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NoFieldSelectors #-}

module Comments.Api (
  Api, 
  Comments (..), 
  IncomingComment (..),
  Links,
  makeLinks,
  ) where

import Data.Text (Text)
import GHC.Generics
import GHC.Generics (Generic)
import Lucid
import Servant.API
import Servant.API.ContentTypes.Lucid
import Servant.API.NamedRoutes
import Web.FormUrlEncoded
import Servant.Links
import Network.URI
import Data.Function ((&))
import Data.Proxy

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

type Links = Comments (AsLink URI)

-- | Create a links struct with absolute URIs
-- https://hachyderm.io/@DiazCarrete/111841132226571708
-- https://www.youtube.com/watch?v=KC64Ymo63hQ
makeLinks :: IO Links
makeLinks = do
  root <- parseRelativeReference "/" & maybe (fail "Could not create root URI") pure
  pure $ allLinks' (\r -> linkURI r `relativeTo` root) (Proxy @Api)
