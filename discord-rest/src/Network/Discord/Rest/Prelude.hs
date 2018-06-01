{-# LANGUAGE DataKinds, OverloadedStrings, MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes, FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE FunctionalDependencies, KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Utility and base types and functions for the Discord Rest API
module Network.Discord.Rest.Prelude where

import Data.Monoid ((<>))
import Network.HTTP.Req ((=:))
import Data.Aeson
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Network.HTTP.Req as R

import Network.Discord.Types

class DiscordRequest r where
  compileJsonRequest :: FromJSON req => r -> IO (JsonRequest req)
  majorRoute     :: r -> T.Text


-- | The base url (Req) for API requests
baseUrl :: R.Url 'R.Https
baseUrl = R.https "discordapp.com" R./: "api" R./: apiVersion
  where apiVersion = "v6"

authHeader :: DiscordAuth -> R.Option 'R.Https
authHeader (DiscordAuth auth version) =
          R.header "Authorization" (TE.encodeUtf8 (formatAuth auth))
       <> R.header "User-Agent" agent
  where
  srcUrl = "https://github.com/jano017/Discord.hs"
  agent = "DiscordBot (" <> srcUrl <> ", " <> TE.encodeUtf8 version <> ")"

-- Append to an URL
infixl 5 //
(//) :: Show a => R.Url scheme -> a -> R.Url scheme
url // part = url R./: T.pack (show part)


type Option = R.Option 'R.Https

-- | Represtents a HTTP request made to an API that supplies a Json response
data JsonRequest r where
  Delete ::  FromJSON r                => R.Url 'R.Https      -> R.Option 'R.Https -> JsonRequest r
  Get    ::  FromJSON r                => R.Url 'R.Https      -> R.Option 'R.Https -> JsonRequest r
  Patch  :: (FromJSON r, R.HttpBody a) => R.Url 'R.Https -> a -> R.Option 'R.Https -> JsonRequest r
  Post   :: (FromJSON r, R.HttpBody a) => R.Url 'R.Https -> a -> R.Option 'R.Https -> JsonRequest r
  Put    :: (FromJSON r, R.HttpBody a) => R.Url 'R.Https -> a -> R.Option 'R.Https -> JsonRequest r

-- | Represents a range of 'Snowflake's
data Range = Range { after :: Snowflake, before :: Snowflake, limit :: Int}

defaultRange :: Range
defaultRange = Range 0 18446744073709551615 100
--                       2^64 - 1

-- | Convert a Range to a query string
toQueryString :: Range -> Option
toQueryString (Range a b l)
  =  "after"  =: show a
  <> "before" =: show b
  <> "limit"  =: show l

--instance (MonadIO m, DiscordRest m) => MonadHttp m where
--  handleHttpException = liftIO . throwIO

--fetch :: (FromJSON r) => JsonRequest r -> IO (R.JsonResponse r)
--fetch (Delete url      opts) = R.req R.DELETE url R.NoReqBody R.jsonResponse =<< (<> opts) <$> baseRequestOptions
--fetch (Get    url      opts) = R.req R.GET    url R.NoReqBody R.jsonResponse =<< (<> opts) <$> baseRequestOptions
--fetch (Patch  url body opts) = R.req R.PATCH  url body        R.jsonResponse =<< (<> opts) <$> baseRequestOptions
--fetch (Post   url body opts) = R.req R.POST   url body        R.jsonResponse =<< (<> opts) <$> baseRequestOptions
--fetch (Put    url body opts) = R.req R.PUT    url body        R.jsonResponse =<< (<> opts) <$> baseRequestOptions

