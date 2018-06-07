{-# LANGUAGE ExistentialQuantification, TypeSynonymInstances, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Provides base types and utility functions needed for modules in Network.Discord.Types
module Network.Discord.Types.Prelude where

import Data.Bits
import Data.Word

import Data.Aeson.Types
import Data.Time.Clock
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as Q
import Data.Time.Clock.POSIX
import Data.Monoid ((<>))
import Control.Monad (mzero)

-- | Authorization token for the Discord API
data Auth = Bot    Q.ByteString
          | Client Q.ByteString
          | Bearer Q.ByteString


-- | Formats the token for use with the REST API
formatAuth :: Auth -> Q.ByteString
formatAuth (Bot    token) = "Bot "    <> token
formatAuth (Client token) = token
formatAuth (Bearer token) = "Bearer " <> token

-- | Get the raw token formatted for use with the websocket gateway
authToken :: Auth -> Q.ByteString
authToken (Bot    token) = token
authToken (Client token) = token
authToken (Bearer token) = token

-- | A unique integer identifier. Can be used to calculate the creation date of an entity.
newtype Snowflake = Snowflake Word64
  deriving (Ord, Eq, Num, Integral, Enum, Real, Bits)

instance Show Snowflake where
  show (Snowflake a) = show a

instance ToJSON Snowflake where
  toJSON (Snowflake snowflake) = String . T.pack $ show snowflake

instance FromJSON Snowflake where
  parseJSON (String snowflake) = Snowflake <$> (return . read $ T.unpack snowflake)
  parseJSON _ = mzero

-- |Gets a creation date from a snowflake.
creationDate :: Snowflake -> UTCTime
creationDate x = posixSecondsToUTCTime . realToFrac
  $ 1420070400 + quot (shiftR x 22) 1000

-- | Default timestamp
epochTime :: UTCTime
epochTime = posixSecondsToUTCTime 0

-- | Convert ToJSON values to FromJSON values
reparse :: (ToJSON a, FromJSON b) => a -> Either String b
reparse val = parseEither parseJSON $ toJSON val
