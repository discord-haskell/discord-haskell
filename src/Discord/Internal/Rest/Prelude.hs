{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

-- | Utility and base types and functions for the Discord Rest API
module Discord.Internal.Rest.Prelude where

import Prelude hiding (log)
import Control.Exception.Safe (throwIO)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.String (IsString(fromString))
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

import qualified Network.HTTP.Req as R
import Web.Internal.HttpApiData (ToHttpApiData)

import Discord.Internal.Types

import Paths_discord_haskell (version)
import Data.Version (showVersion)

-- | The api version to use.
apiVersion :: T.Text
apiVersion = "10"

-- | The base url (Req) for API requests
baseUrl :: R.Url 'R.Https
baseUrl = R.https "discord.com" R./: "api" R./: apiVersion'
  where apiVersion' = "v" <> apiVersion

-- | Discord requires HTTP headers for authentication.
authHeader :: Auth -> R.Option 'R.Https
authHeader auth =
          R.header "Authorization" (TE.encodeUtf8 (authToken auth))
       <> R.header "User-Agent" agent
  where
  -- | https://discord.com/developers/docs/reference#user-agent
  -- Second place where the library version is noted
  agent = fromString $ "DiscordBot (https://github.com/discord-haskell/discord-haskell, " <> showVersion version <> ")"

-- Possibly append to an URL
infixl 5 /?
(/?) :: ToHttpApiData a => R.Url scheme -> Maybe a -> R.Url scheme
(/?) url Nothing = url
(/?) url (Just part) = url R./~ part


-- | A compiled HTTP request ready to execute
data JsonRequest where
  Delete ::                 R.Url 'R.Https ->      R.Option 'R.Https -> JsonRequest
  Get    ::                 R.Url 'R.Https ->      R.Option 'R.Https -> JsonRequest
  Put    :: R.HttpBody a => R.Url 'R.Https -> a -> R.Option 'R.Https -> JsonRequest
  Patch  :: R.HttpBody a => R.Url 'R.Https -> RestIO a -> R.Option 'R.Https -> JsonRequest
  Post   :: R.HttpBody a => R.Url 'R.Https -> RestIO a -> R.Option 'R.Https -> JsonRequest

class Request a where
  -- | used for putting a request into a rate limit bucket
  --   https://discord.com/developers/docs/topics/rate-limits#rate-limits
  majorRoute :: a -> String

  -- | build a JSON http request
  jsonRequest :: a -> JsonRequest

-- | Same Monad as IO. Overwrite Req settings
newtype RestIO a = RestIO { restIOtoIO :: IO a }
  deriving (Functor, Applicative, Monad, MonadIO)

instance R.MonadHttp RestIO where
  -- | Throw actual exceptions
  handleHttpException = liftIO . throwIO
  -- | Don't throw exceptions on http error codes like 404
  getHttpConfig = pure $ R.defaultHttpConfig { R.httpConfigCheckResponse = \_ _ _ -> Nothing }
