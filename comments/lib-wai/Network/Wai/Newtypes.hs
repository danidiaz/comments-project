{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Network.Wai.Newtypes (
    Application_,
    fromApplication,
    getApplication,
    Middleware_,
    fromMiddleware,
    applyMiddleware
) where

-- | Skeleton for Wai.Newtypes module

import Network.Wai

newtype Application_ = Application_ Application

fromApplication :: Application -> Application_
fromApplication app = Application_ app

getApplication :: Application_ -> Application
getApplication (Application_ app) = app

newtype Middleware_ = Middleware_ Middleware

fromMiddleware :: Middleware -> Middleware_
fromMiddleware mw = Middleware_ mw

applyMiddleware :: Middleware_ -> Application_ -> Application_
applyMiddleware (Middleware_ mw) (Application_ app) = Application_ (mw app)

