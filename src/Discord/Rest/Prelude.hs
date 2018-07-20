{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

-- | Utility and base types and functions for the Discord Rest API
module Discord.Rest.Prelude where

import Prelude hiding (log)
import Data.Default (def)
import Control.Exception (throwIO)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Monoid ((<>))
import qualified Data.Text as T
import qualified Network.HTTP.Req as R

import Discord.Types

-- | Discord requires HTTP headers for authentication.
authHeader :: Auth -> R.Option 'R.Https
authHeader auth =
          R.header "Authorization" (formatAuth auth)
       <> R.header "User-Agent" agent
  where
  agent = "DiscordBot (https://github.com/aquarial/discord-haskell, 0.3.0)"

-- Append to an URL
infixl 5 //
(//) :: Show a => R.Url scheme -> a -> R.Url scheme
(//) url part = url R./: T.pack (show part)


type Option = R.Option 'R.Https

-- | Represtents a HTTP request made to an API that supplies a Json response
data JsonRequest where
  Delete ::                 R.Url 'R.Https ->      Option -> JsonRequest
  Get    ::                 R.Url 'R.Https ->      Option -> JsonRequest
  Patch  :: R.HttpBody a => R.Url 'R.Https -> a -> Option -> JsonRequest
  Put    :: R.HttpBody a => R.Url 'R.Https -> a -> Option -> JsonRequest
  Post   :: R.HttpBody a => R.Url 'R.Https -> RestIO a -> Option -> JsonRequest

-- | Same Monad as IO. Overwrite Req settings
newtype RestIO a = RestIO { restIOtoIO :: IO a }
  deriving (Functor, Applicative, Monad, MonadIO)

instance R.MonadHttp RestIO where
  -- | Throw actual exceptions
  handleHttpException = liftIO . throwIO
  -- | Don't throw exceptions on http error codes like 404
  getHttpConfig = pure $ def { R.httpConfigCheckResponse = \_ _ _ -> Nothing }
