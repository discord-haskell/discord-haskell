{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Data structures pertaining to Discord stage instances
module Discord.Internal.Types.StageInstance where

import Data.Aeson
import Data.Data (Data)
import qualified Data.Text as T

import Discord.Internal.Types.Prelude

-- | A live stage in a stage channel. Detailed information can be found at <https://discord.com/developers/docs/resources/stage-instance#stage-instance-object>.
data StageInstance = StageInstance
  { stageInstanceId :: StageId -- ^ Id of the stage instance
  , stageInstanceGuildId :: GuildId -- ^ Guild the stage channel is in
  , stageInstanceChannelId :: ChannelId -- ^ Stage channel the stage instance is in
  , stageInstanceTopic :: T.Text -- ^ Topic of the stage instance, from 1 to 120 characters
  , stageInstancePrivacyLevel :: StageInstancePrivacyLevel -- ^ Who can see the stage instance
  , stageInstanceDiscoverableDisabled :: Bool -- ^ Whether Stage Discovery is disabled. Deprecated by Discord
  , stageInstanceGuildScheduledEventId :: Maybe ScheduledEventId -- ^ Scheduled event linked to the stage instance
  } deriving (Show, Read, Eq, Ord)

instance FromJSON StageInstance where
  parseJSON = withObject "StageInstance" $ \o ->
    StageInstance <$> o .:  "id"
                  <*> o .:  "guild_id"
                  <*> o .:  "channel_id"
                  <*> o .:  "topic"
                  <*> o .:  "privacy_level"
                  <*> o .:  "discoverable_disabled"
                  <*> o .:? "guild_scheduled_event_id"

-- | Who can see a stage instance
data StageInstancePrivacyLevel
  -- | Visible to the public. Deprecated by Discord
  = StageInstancePrivacyLevelPublic
  -- | Visible only to the members of the guild
  | StageInstancePrivacyLevelGuildOnly
  deriving (Show, Read, Eq, Ord, Data)

instance InternalDiscordEnum StageInstancePrivacyLevel where
  discordTypeStartValue = StageInstancePrivacyLevelPublic
  fromDiscordType StageInstancePrivacyLevelPublic = 1
  fromDiscordType StageInstancePrivacyLevelGuildOnly = 2

instance FromJSON StageInstancePrivacyLevel where
  parseJSON = discordTypeParseJSON "StageInstancePrivacyLevel"

instance ToJSON StageInstancePrivacyLevel where
  toJSON = toJSON . fromDiscordType
