{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Discord.Internal.Types.AuditLog 
  ( AuditLog (..)
  , AuditLogEntry (..)
  , AuditLogChange (..)
  , AuditLogEntryOptions
  , AuditLogTiming (..)
  , GetAuditLogOpts (..)
  , AuditLogEvent (..)
  , toAuditLogEvent
  ) where


import Data.Aeson

import Discord.Internal.Types.Guild (Integration)
import Discord.Internal.Types.Prelude
import Discord.Internal.Types.User
import Discord.Internal.Types.ScheduledEvents
import Discord.Internal.Types.Channel
import Discord.Internal.Types.ApplicationCommands
import Discord.Internal.Types.AutoModeration
import Data.Default (Default(..))



-- | Audit log object, along with the entries it also contains referenced users, integrations [...] and so on
data AuditLog = AuditLog
  { auditLogEntries             :: [AuditLogEntry]
  , auditLogUsers               :: [User]
  , auditLogIntegrations        :: [Integration]
  , auditLogWebhooks            :: [Webhook]
  , auditlogScheduledEvents     :: [ScheduledEvent]
  , auditLogThreads             :: [Channel]
  , auditLogApplicationCommands :: [ApplicationCommand]
  , auditLogAutoModerationRules :: [AutoModerationRule]
  } deriving ( Eq, Show, Read )


instance FromJSON AuditLog where
  parseJSON = withObject "AuditLog" $ \o ->
    AuditLog <$> o .: "audit_log_entries"
             <*> o .: "users"
             <*> o .: "integrations"
             <*> o .: "webhooks"
             <*> o .: "guild_scheduled_events"
             <*> o .: "threads"
             <*> o .: "application_commands"
             <*> o .: "auto_moderation_rules"

-- | An audit log entry object, so to speak the actual event that took place
data AuditLogEntry = AuditLogEntry
  { auditLogEntryId         :: AuditLogEntryId
  , auditLogEntryActionType :: AuditLogEvent
  , auditLogEntryUserId     :: Maybe UserId
  , auditLogEntryTargetId   :: Maybe Snowflake
  , auditLogEntryChanges    :: Maybe [AuditLogChange]
  , auditLogEntryOptions    :: Maybe AuditLogEntryOptions
  , auditLogEntryReasons    :: Maybe String
  } deriving ( Eq, Show, Read )

instance FromJSON AuditLogEntry where
  parseJSON = withObject "AuditLogEntry" $ \o ->
    AuditLogEntry <$> o .:  "id"
                  <*> o .:  "action_type"
                  <*> o .:? "user_id"
                  <*> o .:? "target_id"
                  <*> o .:? "changes"
                  <*> o .:? "options"
                  <*> o .:? "reasons"

newtype AuditLogEvent = MkAuditLogEvent Int
  deriving ( Eq, Show, Read )

instance FromJSON AuditLogEvent where
  parseJSON = fmap MkAuditLogEvent . parseJSON

-- | A change object, new value and old value fields are of Aesons `Value` type,
--   because it can be pretty much any value from discord api
data AuditLogChange = AuditLogChange
  { auditLogChangeKey      :: String
  , auditLogChangeNewValue :: Maybe Value
  , auditLogChangeOldValue :: Maybe Value
  } deriving ( Eq, Show, Read )

instance FromJSON AuditLogChange where
  parseJSON = withObject "AuditLogChange" $ \o ->
    AuditLogChange <$> o .:  "key"
                   <*> o .:? "new_value"
                   <*> o .:? "old_value"

-- | Optional data for the Audit Log Entry object
data AuditLogEntryOptions = AuditLogEntryOptions
  { auditLogEntryOptionApplicationId                  :: Maybe ApplicationId
  , auditLogEntryOptionAutoModerationRuleName         :: Maybe String
  , auditLogEntryOptionAutoModerationRuleTriggerType  :: Maybe String
  , auditLogEntryOptionChannelId                      :: Maybe ChannelId
  , auditLogEntryOptionCount                          :: Maybe String
  , auditLogEntryOptionDeleteMemberDays               :: Maybe String
  , auditLogEntryOptionId                             :: Maybe Snowflake
  , auditLogEntryOptionMembersRemoved                 :: Maybe String
  , auditLogEntryOptionMessageId                      :: Maybe MessageId
  , auditLogEntryOptionRoleName                       :: Maybe String
  , auditLogEntryOptionType                           :: Maybe String
  , auditLogEntryOptionIntegrationType                :: Maybe String
  } deriving ( Eq, Show, Read )

instance FromJSON AuditLogEntryOptions where
  parseJSON = withObject "AuditLogEntryOption" $ \o ->
    AuditLogEntryOptions <$> o .:? "application_id"
                         <*> o .:? "auto_moderation_rule_name"
                         <*> o .:? "auto_moderation_rule_trigger_type"
                         <*> o .:? "channel_id"
                         <*> o .:? "count"
                         <*> o .:? "delete_member_days"
                         <*> o .:? "id"
                         <*> o .:? "members_removed"
                         <*> o .:? "message_id"
                         <*> o .:? "role_name"
                         <*> o .:? "type"
                         <*> o .:? "integration_type"

-- | Options for `GetAuditLog` request
data GetAuditLogOpts = GetAuditLogOpts
  { getAuditLogUserId     :: Maybe UserId
  , getAuditLogActionType :: Maybe AuditLogEvent
  , getAuditLogTiming     :: Maybe AuditLogTiming
  , getAuditLogLimit      :: Maybe Int
  } deriving ( Eq, Show, Read)

instance Default GetAuditLogOpts where
  def = GetAuditLogOpts { getAuditLogUserId     = Nothing
                        , getAuditLogActionType = Nothing
                        , getAuditLogTiming     = Nothing
                        , getAuditLogLimit      = Nothing
                        }

data AuditLogTiming = BeforeLogEntry AuditLogEntryId
                    | AfterLogEntry AuditLogEntryId
                    | LatestLogEntries
  deriving ( Show, Read, Eq, Ord)

-- | See <https://discord.com/developers/docs/resources/audit-log#audit-log-entry-object-audit-log-events> for more information on Events
toAuditLogEvent :: Int -> Maybe AuditLogEvent
toAuditLogEvent i = if i `elem` (1 : 121 : [10..15] <> [20..42] <> [50..52] <> [60..62] <> [72..75] <> [80..85] <> [90..92] <> [100..102] <> [110..112] <> [140..145] <> [150, 151])
  then Just (MkAuditLogEvent i) else Nothing
