{-# LANGUAGE OverloadedStrings #-}

-- | Module vendored from https://hackage.haskell.org/package/servant-lucid-0.9.0.6
--
-- Some helper functions for creating values in
-- [lucid](https://hackage.haskell.org/package/lucid) DSLs that work
-- with [servant](https://hackage.haskell.org/package/servant).
module Lucid.Servant
  ( safeHref_,
    safeAbsHref_,
    safeRelHref_,
    linkHref_,
    linkAbsHref_,
    linkRelHref_,
  )
where

import Data.Proxy
  ( Proxy,
  )
import Data.Semigroup
  ( (<>),
  )
import Data.Text qualified as T
import Lucid
  ( Attributes,
  )
import Lucid.Html5
  ( href_,
  )
import Servant.API
  ( toUrlPiece,
  )
import Servant.Links
  ( HasLink,
    IsElem,
    Link,
    MkLink,
    safeLink',
  )

-- | 'safeLink' variant which creates lucid's 'Attribute' given base url.
--
-- >>> type API = "path" :> Get '[JSON] Int
-- >>> let api = Proxy :: Proxy API
--
-- >>> safeHref_ "" api api
-- Attribute "href" "path"
--
-- >>> safeHref_ "/" api api
-- Attribute "href" "/path"
--
-- >>> safeHref_ "http://example.com" api api
-- Attribute "href" "http://example.com/path"
--
-- >>> safeHref_ "http://example.com/" api api
-- Attribute "href" "http://example.com/path"
safeHref_ ::
  (IsElem endpoint api, HasLink endpoint) =>
  T.Text ->
  Proxy api ->
  Proxy endpoint ->
  MkLink endpoint Attributes
safeHref_ = safeLink' . linkHref_

-- | @'safeLink' "/"@
safeAbsHref_ ::
  (IsElem endpoint api, HasLink endpoint) =>
  Proxy api ->
  Proxy endpoint ->
  MkLink endpoint Attributes
safeAbsHref_ = safeLink' linkAbsHref_

-- | @'safeLink' ""@
safeRelHref_ ::
  (IsElem endpoint api, HasLink endpoint) =>
  Proxy api ->
  Proxy endpoint ->
  MkLink endpoint Attributes
safeRelHref_ = safeLink' linkRelHref_

-- | Create an `href` attribute from a 'Link', with given base url.
--
-- "servant" ensures that any 'Link' is valid within an API.
-- This function ensures it is possible to navigate to that endpoint from
-- a page which shares a root with that API.
linkHref_ :: T.Text -> Link -> Attributes
linkHref_ burl = href_ . (burl <+>) . toUrlPiece

-- | Create an `href` attribute from a 'Link', with leading '/'.
--
-- "servant" ensures that any 'Link' is valid within an API.
-- This function ensures it is possible to navigate to that endpoint from
-- a page which shares a root with that API.
linkAbsHref_ :: Link -> Attributes
linkAbsHref_ = linkHref_ "/"

-- | Create an `href` attribute from a 'Link', as a relative link.
--
-- "servant" ensures that any 'Link' is valid within an API.
-- Use this function if a relative link (no leading '/') is required.
linkRelHref_ :: Link -> Attributes
linkRelHref_ = href_ . toUrlPiece

(<+>) :: T.Text -> T.Text -> T.Text
burl <+> path
  | T.null burl = path
  | T.last burl == '/' = burl <> path
  | otherwise = burl <> "/" <> path
