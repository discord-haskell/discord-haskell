{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

-- | Utility and base types and functions for the Discord Rest API
module Discord.Internal.Rest.Prelude where

import Prelude hiding (log)
import Control.Exception.Safe (throwIO)
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
-- import qualified Data.ByteString as B
-- import Network.HTTP.Req (ReqBodyMultipart)
-- import qualified Data.ByteString.Lazy as BL
-- import Data.Aeson
-- import Network.HTTP.Client.MultipartFormData (partBS, partFileRequestBody)
-- import Network.HTTP.Client (RequestBody(RequestBodyBS))


import qualified Network.HTTP.Req as R

import Discord.Internal.Types

-- | The base url (Req) for API requests
baseUrl :: R.Url 'R.Https
baseUrl = R.https "discord.com" R./: "api" R./: apiVersion
  where apiVersion = "v8"

-- | Discord requires HTTP headers for authentication.
authHeader :: Auth -> R.Option 'R.Https
authHeader auth =
          R.header "Authorization" (TE.encodeUtf8 (authToken auth))
       <> R.header "User-Agent" agent
  where
  -- | https://discord.com/developers/docs/reference#user-agent
  -- Second place where the library version is noted
  agent = "DiscordBot (https://github.com/aquarial/discord-haskell, 1.12.0)"

-- Append to an URL
infixl 5 //
(//) :: Show a => R.Url scheme -> a -> R.Url scheme
(//) url part = url R./: T.pack (show part)


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

-- addAttachments :: ToJSON a => a -> [(T.Text ,B.ByteString)] -> RestIO ReqBodyMultipart
-- addAttachments a attachments = R.reqBodyMultipart $ payloadPart: ((\(i, (nm, content)) -> partFileRequestBody ("file[" <> T.pack (show i) <> "]") (T.unpack nm) (RequestBodyBS content)) <$> zip [0..] attachments)
--   where
--     payloadPart = partBS "payload_json" $ BL.toStrict $ encode a
