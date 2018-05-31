{-# LANGUAGE GADTs, OverloadedStrings, InstanceSigs, TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds, ScopedTypeVariables, Rank2Types #-}
-- | Provide HTTP primitives
module Network.Discord.Rest.HTTP
  ( JsonRequest(..)
  , R.ReqBodyJson(..)
  , R.NoReqBody(..)
  , baseUrl
  , fetch
  , makeRequest
  , (R./:)
  ) where

import Data.Semigroup ((<>))

import Control.Monad (when)
import Control.Concurrent.Chan
import Data.Aeson
import Data.ByteString.Char8 (pack, ByteString)
import Data.Hashable
import Data.Maybe (fromMaybe)
import qualified Data.Text as T (pack)
import qualified Network.HTTP.Req as R

import Network.Discord.Rest.Prelude
import Network.Discord.Types


type Option = R.Option 'R.Https

-- | Represtents a HTTP request made to an API that supplies a Json response
data JsonRequest r where
  Delete ::  FromJSON r                => R.Url 'R.Https      -> Option -> JsonRequest r
  Get    ::  FromJSON r                => R.Url 'R.Https      -> Option -> JsonRequest r
  Patch  :: (FromJSON r, R.HttpBody a) => R.Url 'R.Https -> a -> Option -> JsonRequest r
  Post   :: (FromJSON r, R.HttpBody a) => R.Url 'R.Https -> a -> Option -> JsonRequest r
  Put    :: (FromJSON r, R.HttpBody a) => R.Url 'R.Https -> a -> Option -> JsonRequest r

fetch :: (FromJSON r, DiscordRest m) => JsonRequest r -> m (R.JsonResponse r)
fetch (Delete url      opts) = R.req R.DELETE url R.NoReqBody R.jsonResponse =<< (<> opts) <$> baseRequestOptions
fetch (Get    url      opts) = R.req R.GET    url R.NoReqBody R.jsonResponse =<< (<> opts) <$> baseRequestOptions
fetch (Patch  url body opts) = R.req R.PATCH  url body        R.jsonResponse =<< (<> opts) <$> baseRequestOptions
fetch (Post   url body opts) = R.req R.POST   url body        R.jsonResponse =<< (<> opts) <$> baseRequestOptions
fetch (Put    url body opts) = R.req R.PUT    url body        R.jsonResponse =<< (<> opts) <$> baseRequestOptions

makeRequest :: (FromJSON r, DiscordRest m, DoFetch f r)
  => f r -> JsonRequest r -> m r
makeRequest req action = do
  waitRateLimit req
  resp <- fetch action
  when (parseHeader resp "X-RateLimit-Remaining" 1 < 1) $
    setRateLimit req $ parseHeader resp "X-RateLimit-Reset" 0
  return $ R.responseBody resp
  where
    parseHeader :: R.HttpResponse resp => resp -> ByteString -> Int -> Int
    parseHeader resp header def = fromMaybe def $ decodeStrict =<< R.responseHeader resp header

instance Hashable (JsonRequest r) where
  hashWithSalt s (Delete url _)   = hashWithSalt s $ show url
  hashWithSalt s (Get    url _)   = hashWithSalt s $ show url
  hashWithSalt s (Patch  url _ _) = hashWithSalt s $ show url
  hashWithSalt s (Post   url _ _) = hashWithSalt s $ show url
  hashWithSalt s (Put    url _ _) = hashWithSalt s $ show url

-- | Base implementation of DoFetch, allows arbitrary HTTP requests to be performed
instance (FromJSON r) => DoFetch JsonRequest r where
  doFetch req = R.responseBody <$> fetch req
