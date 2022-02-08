{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE RankNTypes  #-}

-- | Provides base types and utility functions needed for modules in Discord.Internal.Types
module Discord.Internal.Types.Prelude where

import Data.Bits
import Data.Word

import Data.Aeson.Types
import Data.Time.Clock
import qualified Data.Text as T
import Data.Time.Clock.POSIX

import Data.Functor.Compose (Compose(Compose, getCompose))
import Data.Bifunctor (first)
import Text.Read (readMaybe)
import Data.Data (Data (dataTypeOf), dataTypeConstrs, fromConstr)

-- | Authorization token for the Discord API
newtype Auth = Auth T.Text
  deriving (Show, Read, Eq, Ord)


-- | Get the raw token formatted for use with the websocket gateway
authToken :: Auth -> T.Text
authToken (Auth tok) = let token = T.strip tok
                           bot = if "Bot " `T.isPrefixOf` token then "" else "Bot "
                       in bot <> token

-- | A unique integer identifier. Can be used to calculate the creation date of an entity.
newtype Snowflake = Snowflake Word64
  deriving (Ord, Eq, Num, Integral, Enum, Real, Bits)

instance Show Snowflake where
  show (Snowflake a) = show a

instance Read Snowflake where
  readsPrec p = getCompose $ first Snowflake <$> Compose (readsPrec p)

instance ToJSON Snowflake where
  toJSON (Snowflake snowflake) = String . T.pack $ show snowflake

instance FromJSON Snowflake where
  parseJSON =
    withText
      "Snowflake"
      ( \snowflake ->
          case readMaybe (T.unpack snowflake) of
            Nothing -> fail "null snowflake"
            (Just i) -> pure i
      )

type ChannelId = Snowflake
type StageId = Snowflake
type GuildId = Snowflake
type MessageId = Snowflake
type AttachmentId = Snowflake
type EmojiId = Snowflake
type StickerId = Snowflake
type UserId = Snowflake
type OverwriteId = Snowflake
type RoleId = Snowflake
type IntegrationId = Snowflake
type WebhookId = Snowflake
type ParentId = Snowflake
type ApplicationId = Snowflake
type ApplicationCommandId = Snowflake
type InteractionId = Snowflake
type InteractionToken = T.Text
type Shard = (Int, Int)

-- | Gets a creation date from a snowflake.
snowflakeCreationDate :: Snowflake -> UTCTime
snowflakeCreationDate x = posixSecondsToUTCTime . realToFrac
  $ 1420070400 + quot (shiftR x 22) 1000

-- | Default timestamp
epochTime :: UTCTime
epochTime = posixSecondsToUTCTime 0

{-

InternalDiscordEnum is a hack-y typeclass, but it's the best solution overall.
The best we can do is prevent the end-user from seeing this.

typeclass Bounded (minBound + maxBound) could replace discordTypeStartValue, but
it can't derive instances for types like DiscordColor, which have simple sum types involved.

typeclass Enum (toEnum + fromEnum) requires defining both A->Int and Int->A.
If we handle both at once (with an inline map), it's no longer typesafe.

External packages exist, but bloat our dependencies

-}
class Data a => InternalDiscordEnum a where
  discordTypeStartValue :: a
  fromDiscordType :: a -> Int
  discordTypeTable :: [(Int, a)]
  discordTypeTable =  map (\d -> (fromDiscordType d, d)) (makeTable discordTypeStartValue)
    where
      makeTable :: Data b => b -> [b]
      makeTable t = map fromConstr (dataTypeConstrs $ dataTypeOf t)

  discordTypeParseJSON :: String -> Value -> Parser a
  discordTypeParseJSON name =
    withScientific
      name
      ( \i -> do
          case maybeInt i >>= (`lookup` discordTypeTable) of
            Nothing -> fail $ "could not parse type: " ++ show i
            Just d -> return d
      )
    where
      maybeInt i
        | fromIntegral (round i) == i = Just $ round i
        | otherwise = Nothing

toMaybeJSON :: (ToJSON a) => a -> Maybe Value
toMaybeJSON = return . toJSON
