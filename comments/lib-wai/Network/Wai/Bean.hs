{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE NoFieldSelectors #-}

module Network.Wai.Bean
  ( Application_ (..),
    Middleware_ (..),
    applyMiddleware_,
  )
where

import Data.Monoid (Dual (..), Endo (..))
import Network.Wai

newtype Application_ = Application_ {application :: Application}

-- | In the 'Semigroup' instance, the left 'Middleware_' will be applied /first/ to the 'Application_'!
newtype Middleware_ = Middleware_ {middleware :: Middleware}
  deriving (Semigroup, Monoid) via (Dual (Endo Application))

applyMiddleware_ :: Middleware_ -> Application_ -> Application_
applyMiddleware_ (Middleware_ {middleware}) (Application_ {application}) =
  Application_ {application = middleware application}
