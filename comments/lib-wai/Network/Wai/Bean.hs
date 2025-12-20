{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE NoFieldSelectors #-}

module Network.Wai.Bean
  ( Application (..),
    Middleware (..),
    applyMiddleware,
  )
where

import Data.Monoid (Dual (..), Endo (..))
import Network.Wai qualified

newtype Application = Application {application :: Network.Wai.Application}

-- | In the 'Semigroup' instance, the left 'Middleware' will be applied /first/ to the 'Application'!
newtype Middleware = Middleware {middleware :: Network.Wai.Middleware}
  deriving (Semigroup, Monoid) via (Dual (Endo Application))

applyMiddleware :: Middleware -> Application -> Application
applyMiddleware (Middleware {middleware}) (Application {application}) =
  Application {application = middleware application}
