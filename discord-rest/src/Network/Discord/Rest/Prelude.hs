{-# LANGUAGE DataKinds, OverloadedStrings, MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes, FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Utility and base types and functions for the Discord Rest API
module Network.Discord.Rest.Prelude where

import Data.Monoid ((<>))
import Data.Aeson
import qualified Data.Text as T

import Network.HTTP.Simple

import Network.Discord.Types

class DiscordRequest req where
  majorRoute    :: req a -> T.Text
  modifyRequest :: FromJSON r => req r -> (Request -> Request)


-- | The base url (Req) for API requests
baseUrl :: T.Text
baseUrl = "https://discordapp.com/api/v6/"

authHeader :: DiscordAuth -> (Request -> Request)
authHeader (DiscordAuth auth version) =
          setRequestHeader "Authorization" [formatAuth auth]
        . setRequestHeader "User-Agent" ["DiscordBot (" <> srcUrl <> ", "
                                                        <> version <> ") "
                                                        <> "Currently forking and rewriting"]
  where
  srcUrl = "https://github.com/aquarial/Discord.hs"


-- | Represents a range of 'Snowflake's
data Range = Range { after :: Snowflake, before :: Snowflake, limit :: Int}

maxRange :: Range
maxRange = Range 0 18446744073709551615 100
--                       2^64 - 1

-- -- | Convert a Range to a query string
-- toQueryString :: Range -> Option
-- toQueryString (Range a b l)
--   =  "after"  =: show a
--   <> "before" =: show b
--   <> "limit"  =: show l
--
