{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Discord.Internal.Types.AutoModeration where

import Data.Aeson
import Discord.Internal.Types.Prelude
import Data.Scientific ( toBoundedInteger )


data AutoModerationRule = AutoModerationRule
  { autoModerationRuleId              :: AutoModerationRuleId
  , autoModerationRuleGuildId         :: GuildId
  , autoModerationRuleName            :: String
  , autoModerationRuleCreatorId       :: UserId
  , autoModerationRuleEventType       :: AutoModerationRuleEventType
  , autoModerationRuleTriggerType     :: AutoModerationRuleTriggerType
  , autoModerationRuleTriggerMetadata :: AutoModerationRuleTriggerMetadata
  , autoModerationRuleActions         :: [AutoModerationRuleAction]
  , autoModerationRuleEnabled         :: Bool
  , autoModerationRuleExemptRoles     :: [RoleId]
  , autoModerationRuleExemptChannels  :: [ChannelId]
  } deriving ( Eq, Show, Read )

instance FromJSON AutoModerationRule where
  parseJSON = withObject "AutoModerationRule" $ \o ->
    AutoModerationRule <$> o .: "id"
                       <*> o .: "guild_id"
                       <*> o .: "name"
                       <*> o .: "creator_id"
                       <*> o .: "event_type"
                       <*> o .: "trigger_type"
                       <*> o .: "trigger_metadata"
                       <*> o .: "actions"
                       <*> o .: "enabled"
                       <*> o .: "exempt_roles"
                       <*> o .: "exempt_channels"

instance ToJSON AutoModerationRule where
  toJSON AutoModerationRule{..} = object
    [ ("id", toJSON autoModerationRuleGuildId)
    , ("guild_id", toJSON autoModerationRuleGuildId)
    , ("name", toJSON autoModerationRuleName)
    , ("creator_id", toJSON autoModerationRuleCreatorId)
    , ("event_type", toJSON autoModerationRuleEventType)
    , ("trigger_type", toJSON autoModerationRuleTriggerType)
    , ("trigger_metadata", toJSON autoModerationRuleTriggerMetadata)
    , ("actions", toJSON autoModerationRuleActions)
    , ("enabled", toJSON autoModerationRuleEnabled)
    , ("exempt_roles", toJSON autoModerationRuleExemptRoles)
    , ("exempt_channels", toJSON autoModerationRuleExemptChannels)
    ]

-- Discord makes it pretty clear they left room for updates, so far this is the only event type
-- See <https://discord.com/developers/docs/resources/auto-moderation#auto-moderation-rule-object-event-types>
data AutoModerationRuleEventType
  = MessageSent
  deriving ( Eq, Show, Read )

instance FromJSON AutoModerationRuleEventType where
  parseJSON = withScientific "AutoModerationRuleEventType" $ \v -> case toBoundedInteger v :: Maybe Int of
    Just 1 -> return MessageSent
    _      -> fail $ "Could not parse event type: " <> show v

instance ToJSON AutoModerationRuleEventType where
  toJSON MessageSent = Number 1


data AutoModerationRuleTriggerType
  = Keyword
  | Spam
  | KeywordPreset
  | MentionSpam
  deriving ( Eq, Show, Read )

instance FromJSON AutoModerationRuleTriggerType where
  parseJSON = withScientific "AutoModerationTriggerType" $ \v -> case toBoundedInteger v :: Maybe Int of
    Just 1 -> return Keyword
    Just 3 -> return Spam
    Just 4 -> return KeywordPreset
    Just 5 -> return MentionSpam
    _      -> fail $ "Could not parse trigger type: " <> show v

instance ToJSON AutoModerationRuleTriggerType where
  toJSON Keyword        = Number 1
  toJSON Spam           = Number 3
  toJSON KeywordPreset  = Number 4
  toJSON MentionSpam    = Number 5

data AutoModerationRuleTriggerMetadata = AutoModerationRuleTriggerMetadata
  { autoModerationRuleTriggerMetadataKeywordFilter  :: [String]
  , autoModerationRuleTriggerMetadataRegexPatterns  :: [String]
  , autoModerationRuleTriggerMetadataPresets        :: Maybe [AutoModerationRuleTriggerMetadataPreset]
  , autoModerationRuleTriggerMetadataAllowList      :: [String]
  , autoModerationRuleTriggerMetadataMentionLimit   :: Maybe Int
  , autoModerationRuleTriggerMetadataRaidProtection :: Maybe Bool
  } deriving ( Eq, Show, Read )

instance FromJSON AutoModerationRuleTriggerMetadata where
  parseJSON = withObject "AutoModerationRuleTriggerMetadata" $ \o ->
    AutoModerationRuleTriggerMetadata
      <$> o .:  "keyword_filter"
      <*> o .:  "regex_patterns"
      <*> o .:? "presets"
      <*> o .:  "allow_list"
      <*> o .:? "mention_total_limit"
      <*> o .:? "mention_raid_protection_enabled"

instance ToJSON AutoModerationRuleTriggerMetadata where
  toJSON AutoModerationRuleTriggerMetadata {..} = objectFromMaybes
    [ "keyword_filter"  .== autoModerationRuleTriggerMetadataKeywordFilter
    , "regex_patterns"  .== autoModerationRuleTriggerMetadataRegexPatterns
    , "presets"         .=? autoModerationRuleTriggerMetadataPresets
    , "allow_list"      .== autoModerationRuleTriggerMetadataAllowList
    , "mention_total_limit" .=? autoModerationRuleTriggerMetadataMentionLimit
    , "mention_raid_protection_enabled" .=? autoModerationRuleTriggerMetadataRaidProtection
    ]

data AutoModerationRuleTriggerMetadataPreset 
  = Profanity
  | SexualContent
  | Slurs
  deriving ( Eq, Show, Read )

instance FromJSON AutoModerationRuleTriggerMetadataPreset where
  parseJSON = withScientific "AutoModerationRuleTriggerMetadataPreset" $ \v -> case toBoundedInteger v :: Maybe Int of
    Just 1 -> return Profanity
    Just 3 -> return SexualContent
    Just 4 -> return Slurs
    _      -> fail $ "Could not parse preset type: " <> show v

instance ToJSON AutoModerationRuleTriggerMetadataPreset where
  toJSON Profanity      = Number 1
  toJSON SexualContent  = Number 2
  toJSON Slurs          = Number 3

data AutoModerationRuleAction = AutoModerationRuleAction
  { autoModerationRuleActionType      :: AutoModerationRuleActionType
  , autoModerationRuleActionMetadata  :: Maybe AutoModerationRuleActionMetadata
  } deriving ( Eq, Show, Read )

instance FromJSON AutoModerationRuleAction where
  parseJSON = withObject "AutoModerationRuleAction" $ \o ->
    AutoModerationRuleAction
      <$> o .: "type"
      <*> o .: "metadata"

instance ToJSON AutoModerationRuleAction where
  toJSON AutoModerationRuleAction{..} = object 
    [ ("type", toJSON autoModerationRuleActionType)
    , ("metadata", toJSON autoModerationRuleActionMetadata)
    ]

data AutoModerationRuleActionType
  = BlockMessage
  | SendAlertMessage
  | Timeout
  deriving ( Eq, Show, Read )

instance FromJSON AutoModerationRuleActionType where
  parseJSON = withScientific "AutoModerationRuleActionType" $ \v -> case toBoundedInteger v :: Maybe Int of
    Just 1 -> return BlockMessage
    Just 3 -> return SendAlertMessage
    Just 4 -> return Timeout
    _      -> fail $ "Could not parse action type: " <> show v

instance ToJSON AutoModerationRuleActionType where
  toJSON BlockMessage     = Number 1
  toJSON SendAlertMessage = Number 2
  toJSON Timeout          = Number 3

data AutoModerationRuleActionMetadata = AutoModerationRuleActionMetadata
  { autoModerationRuleActionMetadataChannelId       :: Maybe ChannelId
  , autoModerationRuleActionMetadataTimeoutDuration :: Maybe Integer
  , autoModerationRuleActionMetadataCustomMessage   :: Maybe String
  } deriving ( Eq, Show, Read )

instance FromJSON AutoModerationRuleActionMetadata where
  parseJSON = withObject "AutoModerationRuleActionMetadata" $ \o ->
    AutoModerationRuleActionMetadata
      <$> o .:? "channel_id"
      <*> o .:? "duration_seconds"
      <*> o .:? "custom_message"

instance ToJSON AutoModerationRuleActionMetadata where
  toJSON AutoModerationRuleActionMetadata{..} = objectFromMaybes
    [ "channel_id"        .=? autoModerationRuleActionMetadataChannelId
    , "duration_seconds"  .=? autoModerationRuleActionMetadataTimeoutDuration
    , "custom_message"    .=? autoModerationRuleActionMetadataCustomMessage
    ]
