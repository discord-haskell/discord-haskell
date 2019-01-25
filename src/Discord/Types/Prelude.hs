{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Provides base types and utility functions needed for modules in Discord.Types
module Discord.Types.Prelude where

import Data.Bits
import Data.Word

import Data.Aeson.Types
import Data.Time.Clock
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Char8 as Q
import Data.Time.Clock.POSIX
import Data.Monoid ((<>))
import Control.Monad (mzero)

-- | Authorization token for the Discord API
data Auth = Auth T.Text
  deriving (Show, Eq, Ord)


-- | Formats the token for use with the REST API
formatAuth :: Auth -> Q.ByteString
formatAuth (Auth givenTok) = let token = T.strip givenTok
                                 bot = if "Bot " `T.isPrefixOf` token then "" else "Bot "
                             in TE.encodeUtf8 $ bot <> token

-- | Get the raw token formatted for use with the websocket gateway
authToken :: Auth -> T.Text
authToken (Auth token) = token

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

type ChannelId = Snowflake
type GuildId = Snowflake
type MessageId = Snowflake
type EmojiId = Snowflake
type UserId = Snowflake
type OverwriteId = Snowflake
type RoleId = Snowflake
type IntegrationId = Snowflake
type WebhookId = Snowflake

-- | Gets a creation date from a snowflake.
snowflakeCreationDate :: Snowflake -> UTCTime
snowflakeCreationDate x = posixSecondsToUTCTime . realToFrac
  $ 1420070400 + quot (shiftR x 22) 1000

-- | Default timestamp
epochTime :: UTCTime
epochTime = posixSecondsToUTCTime 0

