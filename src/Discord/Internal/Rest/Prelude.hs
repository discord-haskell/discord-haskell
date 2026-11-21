{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

-- | Utility and base types and functions for the Discord Rest API
module Discord.Internal.Rest.Prelude where

import Prelude hiding (log)
import Data.String (IsString(fromString))
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Network.HTTP.Types.Header (RequestHeaders, HeaderName)
import Network.HTTP.Client (RequestBody)
import Web.Internal.HttpApiData (ToHttpApiData (toUrlPiece))

import Discord.Internal.Types

import Paths_discord_haskell (version)
import Data.Version (showVersion)
import Network.HTTP.Client.MultipartFormData (Part)

-- | The api version to use.
apiVersion :: T.Text
apiVersion = "10"

-- | The base url (Req) for API requests
baseUrl :: T.Text
baseUrl = "https://discord.com/api/v" <> apiVersion

-- | Discord requires HTTP headers for authentication.
authHeader :: Auth -> RequestHeaders
authHeader auth =
     header "Authorization" ((authToken auth))
  <> header "User-Agent" agent
  where
  -- | https://discord.com/developers/docs/reference#user-agent
  -- Second place where the library version is noted
  agent = "DiscordBot (https://github.com/discord-haskell/discord-haskell, " <> showVersion version <> ")"

-- Possibly append to an URL
infixl 5 /?
(/?) :: ToHttpApiData a => T.Text -> Maybe a -> T.Text
(/?) url Nothing = url
(/?) url (Just part) = url <> "/" <> toUrlPiece part

-- Add a part to an URL (borrowed from `req`)
infixl 5 /~
(/~) :: ToHttpApiData a => T.Text -> a -> T.Text
(/~) url part = url <> "/" <> toUrlPiece part

-- Add a part to an URL, specialized to T.Text (borrowed from `req`)
infixl 5 /:
(/:) :: T.Text -> T.Text -> T.Text
(/:) = (/~)

-- Create a header
header :: (ToHttpApiData a) => T.Text -> a -> [(HeaderName, BS.ByteString)]
header h v = [(fromString $ T.unpack h, TE.encodeUtf8 $ toUrlPiece v)]

-- | A compiled HTTP request ready to execute
data JsonRequest where
  Delete ::  T.Text ->                              RequestHeaders -> JsonRequest
  Get    ::  T.Text ->                              RequestHeaders -> JsonRequest
  Put    ::  T.Text -> Either RequestBody [Part] -> RequestHeaders -> JsonRequest
  Patch  ::  T.Text -> Either RequestBody [Part] -> RequestHeaders -> JsonRequest
  Post   ::  T.Text -> Either RequestBody [Part] -> RequestHeaders -> JsonRequest

class Request a where
  -- | used for putting a request into a rate limit bucket
  --   https://discord.com/developers/docs/topics/rate-limits#rate-limits
  majorRoute :: a -> String

  -- | build a JSON http request
  jsonRequest :: a -> JsonRequest
