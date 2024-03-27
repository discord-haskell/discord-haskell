{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE RankNTypes  #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DerivingStrategies #-}

-- | Provides base types and utility functions needed for modules in Discord.Internal.Types
module Discord.Internal.Types.Prelude
  ( Auth (..)
  , authToken

  , Snowflake (..)
  , snowflakeCreationDate

  , RolePermissions (..)
  
  , DiscordId (..)
  , ChannelId
  , StageId
  , GuildId
  , MessageId
  , AttachmentId
  , EmojiId
  , StickerId
  , UserId
  , DiscordTeamId
  , GameSKUId
  , RoleId
  , IntegrationId
  , WebhookId
  , ParentId
  , ApplicationId
  , ApplicationCommandId
  , InteractionId
  , ScheduledEventId
  , ScheduledEventEntityId
  , AuditLogEntryId
  , AutoModerationRuleId

  , DiscordToken (..)
  , InteractionToken
  , WebhookToken

  , Shard
  , epochTime

  , InternalDiscordEnum (..)

  , Base64Image (..)
  , getMimeType

  , (.==)
  , (.=?)
  , objectFromMaybes

  , ChannelTypeOption (..)
  )

 where

import Data.Bits (Bits(shiftR))
import Data.Data (Data (dataTypeOf), dataTypeConstrs, fromConstr)
import Data.Word (Word64)
import Data.Maybe (catMaybes)
import Text.Read (readMaybe)
import Data.Hashable (Hashable)

import Data.Aeson.Types
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Web.HttpApiData

import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Aeson.Key as Key
import qualified Data.Text.Encoding as T.E

-- | Authorization token for the Discord API
newtype Auth = Auth T.Text
  deriving (Show, Read, Eq, Ord)


-- | Get the raw token formatted for use with the websocket gateway
authToken :: Auth -> T.Text
authToken (Auth tok) = let token = T.strip tok
                           bot = if "Bot " `T.isPrefixOf` token then "" else "Bot "
                       in bot <> token

-- | A unique integer identifier. Can be used to calculate the creation date of an entity.
newtype Snowflake = Snowflake { unSnowflake :: Word64 }
  deriving newtype (Ord, Eq, Hashable, Show, Read, ToJSONKey, FromJSONKey, ToHttpApiData)

instance ToJSON Snowflake where
  toJSON = String . T.pack . show

instance FromJSON Snowflake where
  parseJSON =
    withText
      "Snowflake"
      ( \snowflake ->
          case readMaybe (T.unpack snowflake) of
            Nothing -> fail $ "invalid snowflake: " <> show snowflake
            (Just i) -> pure i
      )

newtype RolePermissions = RolePermissions { getRolePermissions :: Integer } 
  deriving newtype (Eq, Ord, Bits, Read, Show, ToJSON)

-- In v8 and above, all permissions are serialized as strings.
-- See https://discord.com/developers/docs/topics/permissions#permissions.
instance FromJSON RolePermissions where
  parseJSON = withText "RolePermissions" $
      \text -> case readMaybe (T.unpack text) of
              Just perms -> pure $ RolePermissions perms
              Nothing    -> fail $ "invalid role permissions: " <> show text

newtype DiscordId a = DiscordId { unId :: Snowflake }
  deriving newtype (Ord, Eq, Hashable, Show, Read, ToJSON, ToJSONKey, FromJSON, FromJSONKey, ToHttpApiData)

data ChannelIdType
type ChannelId = DiscordId ChannelIdType

data StageIdType
type StageId = DiscordId StageIdType

data GuildIdType
type GuildId = DiscordId GuildIdType

data MessageIdType
type MessageId = DiscordId MessageIdType

data AttachmentIdType
type AttachmentId = DiscordId AttachmentIdType

data EmojiIdType
type EmojiId = DiscordId EmojiIdType

data StickerIdType
type StickerId = DiscordId StickerIdType

data UserIdType
type UserId = DiscordId UserIdType

data RoleIdType
type RoleId = DiscordId RoleIdType

data DiscordTeamIdType
type DiscordTeamId = DiscordId DiscordTeamIdType

data GameSKUIdType
type GameSKUId = DiscordId GameSKUIdType

data IntegrationIdType
type IntegrationId = DiscordId IntegrationIdType

data WebhookIdType
type WebhookId = DiscordId WebhookIdType

data ParentIdType
type ParentId = DiscordId ParentIdType

data ApplicationIdType
type ApplicationId = DiscordId ApplicationIdType

data ApplicationCommandIdType
type ApplicationCommandId = DiscordId ApplicationCommandIdType

data InteractionIdType
type InteractionId = DiscordId InteractionIdType

data ScheduledEventIdType
type ScheduledEventId = DiscordId ScheduledEventIdType

data ScheduledEventEntityIdType
type ScheduledEventEntityId = DiscordId ScheduledEventEntityIdType

data AuditLogEntryIdType
type AuditLogEntryId = DiscordId AuditLogEntryIdType

data AutoModerationRuleIdType
type AutoModerationRuleId = DiscordId AutoModerationRuleIdType

newtype DiscordToken a = DiscordToken { unToken :: T.Text }
  deriving newtype (Ord, Eq, Show, Read, ToJSON, FromJSON, ToHttpApiData)

type InteractionToken = DiscordToken InteractionIdType

type WebhookToken = DiscordToken WebhookIdType

type Shard = (Int, Int)

-- | Gets a creation date from a snowflake.
snowflakeCreationDate :: Snowflake -> UTCTime
snowflakeCreationDate (Snowflake x) = posixSecondsToUTCTime . realToFrac
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

(.==) :: ToJSON a => Key.Key -> a -> Maybe Pair
k .== v = Just (k .= v)

(.=?) :: ToJSON a => Key.Key -> Maybe a -> Maybe Pair
k .=? (Just v) = Just (k .= v)
_ .=? Nothing = Nothing

objectFromMaybes :: [Maybe Pair] -> Value
objectFromMaybes = object . catMaybes


-- | @Base64Image mime data@ represents the base64 encoding of an image (as
-- @data@), together with a tag of its mime type (@mime@).  The constructor is
-- only for Internal use, and its public export is hidden in Discord.Types.
--
-- Public creation of this datatype should be done using the relevant smart
-- constructors for Emoji, Sticker, or Avatar.
data Base64Image a = Base64Image { mimeType :: T.Text, base64Data :: B.ByteString }
  deriving (Show, Read, Eq, Ord)

-- | The ToJSON instance for Base64Image creates a string representation of the
-- image's base-64 data, suited for using as JSON values.
--
-- The format is: @data:%MIME%;base64,%DATA%@.
instance ToJSON (Base64Image a) where
  toJSON (Base64Image mime im) = String $ "data:" <> mime <> ";base64," <> T.E.decodeUtf8 im

-- | @getMimeType bs@ returns a possible mimetype for the given bytestring,
-- based on the first few magic bytes. It may return any of PNG/JPEG/GIF or WEBP
-- mimetypes, or Nothing if none are matched.
--
-- Reference: https://en.wikipedia.org/wiki/List_of_file_signatures
--
-- Although Discord's official documentation does not state WEBP as a supported
-- format, it has been accepted for both emojis and user avatars no problem
-- when tested manually.
--
-- /Inspired by discord.py's implementation./
getMimeType :: B.ByteString -> Maybe T.Text
getMimeType bs
  | B.take 8 bs == "\x89\x50\x4E\x47\x0D\x0A\x1A\x0A"
      = Just "image/png"
  | B.take 3 bs == "\xff\xd8\xff" || B.take 4 (B.drop 6 bs) `elem` ["JFIF", "Exif"]
      = Just "image/jpeg"
  | B.take 6 bs == "\x47\x49\x46\x38\x37\x61" || B.take 6 bs == "\x47\x49\x46\x38\x39\x61"
      = Just "image/gif"
  | B.take 4 bs == "RIFF" && B.take 4 (B.drop 8 bs) == "WEBP"
      = Just "image/webp"
  | otherwise = Nothing

-- | The different channel types. Used for application commands and components.
--
-- https://discord.com/developers/docs/resources/channel#channel-object-channel-types
data ChannelTypeOption
  = -- | A text channel in a server.
    ChannelTypeOptionGuildText
  | -- | A direct message between users.
    ChannelTypeOptionDM
  | -- | A voice channel in a server.
    ChannelTypeOptionGuildVoice
  | -- | A direct message between multiple users.
    ChannelTypeOptionGroupDM
  | -- | An organizational category that contains up to 50 channels.
    ChannelTypeOptionGuildCategory
  | -- | A channel that users can follow and crosspost into their own server.
    ChannelTypeOptionGuildNews
  | -- | A channel in which game developers can sell their game on discord.
    ChannelTypeOptionGuildStore
  | -- | A temporary sub-channel within a guild_news channel.
    ChannelTypeOptionGuildNewsThread
  | -- | A temporary sub-channel within a guild_text channel.
    ChannelTypeOptionGuildPublicThread
  | -- | A temporary sub-channel within a GUILD_TEXT channel that is only
    -- viewable by those invited and those with the MANAGE_THREADS permission
    ChannelTypeOptionGuildPrivateThread
  | -- | A voice channel for hosting events with an audience.
    ChannelTypeOptionGuildStageVoice
  deriving (Show, Read, Data, Eq, Ord)

instance InternalDiscordEnum ChannelTypeOption where
  discordTypeStartValue = ChannelTypeOptionGuildText
  fromDiscordType ChannelTypeOptionGuildText = 0
  fromDiscordType ChannelTypeOptionDM = 1
  fromDiscordType ChannelTypeOptionGuildVoice = 2
  fromDiscordType ChannelTypeOptionGroupDM = 3
  fromDiscordType ChannelTypeOptionGuildCategory = 4
  fromDiscordType ChannelTypeOptionGuildNews = 5
  fromDiscordType ChannelTypeOptionGuildStore = 6
  fromDiscordType ChannelTypeOptionGuildNewsThread = 10
  fromDiscordType ChannelTypeOptionGuildPublicThread = 11
  fromDiscordType ChannelTypeOptionGuildPrivateThread = 12
  fromDiscordType ChannelTypeOptionGuildStageVoice = 13

instance ToJSON ChannelTypeOption where
  toJSON = toJSON . fromDiscordType

instance FromJSON ChannelTypeOption where
  parseJSON = discordTypeParseJSON "ChannelTypeOption"
