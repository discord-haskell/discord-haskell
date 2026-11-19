{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}

-- | Data structures pertaining to gateway dispatch 'Event's
module Discord.Internal.Types.Events where

import Prelude hiding (id)

import Data.Time.ISO8601 (parseISO8601)
import Data.Time (UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Network.Socket (HostName)

import Data.Aeson
import Data.Aeson.Types
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Text as T

import Discord.Internal.Types.Prelude
import Discord.Internal.Types.Channel
import Discord.Internal.Types.Guild
import Discord.Internal.Types.AuditLog (AuditLogEntry)
import Discord.Internal.Types.AutoModeration (AutoModerationRule, AutoModerationRuleAction, AutoModerationRuleTriggerType)
import Discord.Internal.Types.User (User, GuildMember)
import Discord.Internal.Types.Interactions (Interaction)
import Discord.Internal.Types.Emoji (Emoji, Sticker)
import Discord.Internal.Types.ScheduledEvents (ScheduledEvent)


-- | Represents possible events sent by discord. Detailed information can be found at <https://discord.com/developers/docs/topics/gateway>.
data Event =
  -- | Contains the initial state information
    Ready                      Int User [GuildUnavailable] T.Text HostName (Maybe Shard) PartialApplication
  -- | Response to a @Resume@ gateway command
  | Resumed                    [T.Text]
  -- | New auto moderation rule was created, requires the AutoModerationConfiguration intent
  | AutoModerationRuleCreate   AutoModerationRule
  -- | Auto moderation rule was changed, requires the AutoModerationConfiguration intent
  | AutoModerationRuleUpdate   AutoModerationRule
  -- | Auto moderation rule was deleted, requires the AutoModerationConfiguration intent
  | AutoModerationRuleDelete   AutoModerationRule
  -- | Action from an auto moderation rule was executed, requires the AutoModerationExecution intent
  | AutoModerationActionExecution  AutoModerationActionExecuteInfo
  -- | new guild channel created
  | ChannelCreate              Channel
  -- | channel was updated
  | ChannelUpdate              Channel
  -- | channel was deleted
  | ChannelDelete              Channel
  -- | thread created, also sent when being added to a private thread
  | ThreadCreate               Channel
  -- | thread was updated
  | ThreadUpdate               Channel
  -- | thread member for the current user was updated
  | ThreadMemberUpdate         ThreadMemberUpdateFields
  -- | thread was deleted
  | ThreadDelete               Channel
  -- | sent when gaining access to a channel, contains all active threads in that channel
  | ThreadListSync             ThreadListSyncFields
  -- | member or the current user was added or removed from a thread
  | ThreadMembersUpdate        ThreadMembersUpdateFields
  -- | message was pinned or unpinned
  | ChannelPinsUpdate          ChannelId (Maybe UTCTime)
  -- | lazy-load for unavailable guild, guild became available, or user joined a new guild
  | GuildCreate                Guild GuildCreateData
  -- | guild was updated
  | GuildUpdate                Guild
  -- | guild became unavailable, or user left/was removed from a guild
  | GuildDelete                GuildUnavailable
  -- | new entry to the audit log was added
  | GuildAuditLogEntryCreate   GuildId AuditLogEntry
  -- | user was banned from a guild
  | GuildBanAdd                GuildId User
  -- | user was unbanned from a guild
  | GuildBanRemove             GuildId User
  -- | guild emojis were updated
  | GuildEmojiUpdate           GuildId [Emoji]
  -- | guild stickers were updated, contains all of the guild's stickers
  | GuildStickersUpdate        GuildId [Sticker]
  -- | guild integration was updated
  | GuildIntegrationsUpdate    GuildId
  -- | new user joined a guild
  | GuildMemberAdd             GuildId GuildMember
  -- | user was removed from a guild
  | GuildMemberRemove          GuildId User
  -- | guild member was updated
  | GuildMemberUpdate          GuildId [RoleId] User (Maybe T.Text)
  -- | response to @Request Guild Members@ gateway command
  | GuildMemberChunk           GuildId [GuildMember]
  -- | guild role was created
  | GuildRoleCreate            GuildId Role
  -- | guild role was updated
  | GuildRoleUpdate            GuildId Role
  -- | guild role was deleted
  | GuildRoleDelete            GuildId RoleId
  -- | guild scheduled event was created
  | GuildScheduledEventCreate  ScheduledEvent
  -- | guild scheduled event was updated
  | GuildScheduledEventUpdate  ScheduledEvent
  -- | guild scheduled event was deleted
  | GuildScheduledEventDelete  ScheduledEvent
  -- | user subscribed to a guild scheduled event
  | GuildScheduledEventUserAdd ScheduledEventUserInfo
  -- | user unsubscribed from a guild scheduled event
  | GuildScheduledEventUserRemove ScheduledEventUserInfo
  -- | message was created
  | MessageCreate              Message
  -- | message was updated
  | MessageUpdate              MessageUpdateInfo
  -- | message was deleted
  | MessageDelete              MessageDeleteInfo
  -- | multiple messages were deleted at once
  | MessageDeleteBulk          MessageDeleteBulkInfo
  -- | user reacted to a message
  | MessageReactionAdd         ReactionInfo
  -- | user removed a reaction from a message
  | MessageReactionRemove      ReactionInfo
  -- | all reactions were explicitly removed from a message
  | MessageReactionRemoveAll   ReactionRemoveAllInfo
  -- | all reactions for a given emoji were explicitly removed from a message
  | MessageReactionRemoveEmoji ReactionRemoveInfo
  -- | invite to a channel was created, requires the Invites intent
  | InviteCreate               InviteCreateInfo
  -- | invite to a channel was deleted, requires the Invites intent
  | InviteDelete               InviteDeleteInfo
  -- | user voted on a poll
  | MessagePollVoteAdd         PollVoteInfo
  -- | user removed a vote from a poll
  | MessagePollVoteRemove      PollVoteInfo
  -- | user was updated
  | PresenceUpdate             PresenceInfo
  -- | user started typing in a channel
  | TypingStart                TypingInfo
  -- | properties about the user changed
  | UserUpdate                 User
  -- | a webhook of a channel was created, updated, or deleted, requires the Webhooks intent
  | WebhooksUpdate             GuildId ChannelId
  -- | someone joined, left, or moved a voice channel
  | InteractionCreate          Interaction
  --  | VoiceStateUpdate
  --  | VoiceServerUpdate
  -- | An Unknown Event, none of the others
  | UnknownEvent               T.Text Object
  deriving (Show, Eq)

-- | Internal Event representation. Each matches to the corresponding constructor of `Event`.
--
-- An application should never have to use those directly
data EventInternalParse =
    InternalReady                      Int User [GuildUnavailable] T.Text HostName (Maybe Shard) PartialApplication
  | InternalResumed                    [T.Text]
  | InternalAutoModerationRuleCreate   AutoModerationRule
  | InternalAutoModerationRuleUpdate   AutoModerationRule
  | InternalAutoModerationRuleDelete   AutoModerationRule
  | InternalAutoModerationActionExecution AutoModerationActionExecuteInfo
  | InternalChannelCreate              Channel
  | InternalChannelUpdate              Channel
  | InternalChannelDelete              Channel
  | InternalThreadCreate               Channel
  | InternalThreadUpdate               Channel
  | InternalThreadMemberUpdate         ThreadMemberUpdateFields
  | InternalThreadDelete               Channel
  | InternalThreadListSync             ThreadListSyncFields 
  | InternalThreadMembersUpdate        ThreadMembersUpdateFields 
  | InternalChannelPinsUpdate          ChannelId (Maybe UTCTime)
  | InternalGuildCreate                Guild GuildCreateData
  | InternalGuildUpdate                Guild
  | InternalGuildDelete                GuildUnavailable
  | InternalGuildAuditLogEntryCreate   GuildId AuditLogEntry
  | InternalGuildBanAdd                GuildId User
  | InternalGuildBanRemove             GuildId User
  | InternalGuildEmojiUpdate           GuildId [Emoji]
  | InternalGuildStickersUpdate        GuildId [Sticker]
  | InternalGuildIntegrationsUpdate    GuildId
  | InternalGuildMemberAdd             GuildId GuildMember
  | InternalGuildMemberRemove          GuildId User
  | InternalGuildMemberUpdate          GuildId [RoleId] User (Maybe T.Text)
  | InternalGuildMemberChunk           GuildId [GuildMember]
  | InternalGuildRoleCreate            GuildId Role
  | InternalGuildRoleUpdate            GuildId Role
  | InternalGuildRoleDelete            GuildId RoleId
  | InternalGuildScheduledEventCreate  ScheduledEvent
  | InternalGuildScheduledEventUpdate  ScheduledEvent
  | InternalGuildScheduledEventDelete  ScheduledEvent
  | InternalGuildScheduledEventUserAdd ScheduledEventUserInfo
  | InternalGuildScheduledEventUserRemove ScheduledEventUserInfo
  | InternalMessageCreate              Message
  | InternalMessageUpdate              MessageUpdateInfo
  | InternalMessageDelete              MessageDeleteInfo
  | InternalMessageDeleteBulk          MessageDeleteBulkInfo
  | InternalMessageReactionAdd         ReactionInfo
  | InternalMessageReactionRemove      ReactionInfo
  | InternalMessageReactionRemoveAll   ReactionRemoveAllInfo
  | InternalMessageReactionRemoveEmoji ReactionRemoveInfo
  | InternalInviteCreate               InviteCreateInfo
  | InternalInviteDelete               InviteDeleteInfo
  | InternalMessagePollVoteAdd         PollVoteInfo
  | InternalMessagePollVoteRemove      PollVoteInfo
  | InternalPresenceUpdate             PresenceInfo
  | InternalTypingStart                TypingInfo
  | InternalUserUpdate                 User
  | InternalWebhooksUpdate             GuildId ChannelId
  | InternalInteractionCreate          Interaction
  --  | InternalVoiceStateUpdate
  --  | InternalVoiceServerUpdate
  | InternalUnknownEvent               T.Text Object
  deriving (Show, Eq, Read)

-- | Structure containing partial information about an Application
data PartialApplication = PartialApplication
  { partialApplicationID :: ApplicationId
  , partialApplicationFlags :: Int
  } deriving (Show, Eq, Read)

instance FromJSON PartialApplication where
  parseJSON = withObject "PartialApplication" (\v -> PartialApplication <$> v .: "id" <*> v .: "flags")

data GuildCreateData = GuildCreateData
  { guildCreateJoinedAt :: !UTCTime
  , guildCreateLarge :: !Bool
  , guildCreateUnavailable :: !(Maybe Bool)
  , guildCreateMemberCount :: !Int
  -- , guildCreateVoiceStates
  , guildCreateMembers :: ![GuildMember]
  , guildCreateChannels :: ![Channel]
  , guildCreateThreads :: ![Channel]
  , guildCreatePresences :: ![PresenceInfo]
  -- , guildStageInstances :: [StageI]
  , guildCreateScheduledEvents :: ![ScheduledEvent]
  } deriving (Show, Eq, Read)

parseGuildCreate :: Object -> Parser EventInternalParse
parseGuildCreate o = do
  guild :: Guild <- reparse o
  let gid = guildId guild
  channelValues :: [Object] <- o .: "channels"
  threadValues :: [Object] <- o .: "threads"
  let wellFormedChannels = fmap (Object . KM.insert "guild_id" (toJSON gid)) channelValues
      wellFormedThreads = fmap (Object . KM.insert "guild_id" (toJSON gid)) threadValues
  guildCreateData <-
    GuildCreateData <$> o .:  "joined_at"
                    <*> o .:  "large"
                    <*> o .:? "unavailable"
                    <*> o .:  "member_count"
                    <*> o .:  "members"
                    <*> traverse parseJSON wellFormedChannels
                    <*> traverse parseJSON wellFormedThreads
                    <*> o .:  "presences"
                    <*> o .:  "guild_scheduled_events"
  pure $ InternalGuildCreate guild guildCreateData

-- | Structure containing information about a reaction
data ReactionInfo = ReactionInfo
  { reactionUserId    :: UserId -- ^ User who reacted
  , reactionGuildId   :: Maybe GuildId -- ^ Guild in which the reacted message is (if any) 
  , reactionChannelId :: ChannelId -- ^ Channel in which the reacted message is
  , reactionMessageId :: MessageId -- ^ The reacted message
  , reactionEmoji     :: Emoji -- ^ The Emoji used for the reaction
  } deriving (Show, Read, Eq, Ord)

instance FromJSON ReactionInfo where
  parseJSON = withObject "ReactionInfo" $ \o ->
    ReactionInfo <$> o .:  "user_id"
                 <*> o .:? "guild_id"
                 <*> o .:  "channel_id"
                 <*> o .:  "message_id"
                 <*> o .:  "emoji"

-- | Structure containing information about an updated message
data MessageUpdateInfo = MessageUpdateInfo
  { messageUpdateChannelId :: ChannelId -- ^ Channel in which the message is
  , messageUpdateMessageId :: MessageId -- ^ The updated message
  , messageUpdateGuildId   :: Maybe GuildId -- ^ Guild in which the message is (if any)
  } deriving (Show, Read, Eq, Ord)

instance FromJSON MessageUpdateInfo where
  parseJSON = withObject "MessageUpdateInfo" $ \o ->
    MessageUpdateInfo <$> o .:  "channel_id"
                      <*> o .:  "id"
                      <*> o .:? "guild_id"

-- | Structure containing information about a deleted message
data MessageDeleteInfo = MessageDeleteInfo
  { messageDeleteChannelId :: ChannelId -- ^ Channel in which the message was
  , messageDeleteMessageId :: MessageId -- ^ The deleted message
  , messageDeleteGuildId   :: Maybe GuildId -- ^ Guild in which the message was (if any)
  } deriving (Show, Read, Eq, Ord)

instance FromJSON MessageDeleteInfo where
  parseJSON = withObject "MessageDeleteInfo" $ \o ->
    MessageDeleteInfo <$> o .:  "channel_id"
                      <*> o .:  "id"
                      <*> o .:? "guild_id"

-- | Structure containing information about messages that have been deleted at once
data MessageDeleteBulkInfo = MessageDeleteBulkInfo
  { messageDeleteBulkChannelId  :: ChannelId -- ^ Channel in which the messages were
  , messageDeleteBulkMessageIds :: [MessageId] -- ^ The deleted messages
  , messageDeleteBulkGuildId    :: Maybe GuildId -- ^ Guild in which the messages were (if any)
  } deriving (Show, Read, Eq, Ord)

instance FromJSON MessageDeleteBulkInfo where
  parseJSON = withObject "MessageDeleteBulkInfo" $ \o ->
    MessageDeleteBulkInfo <$> o .:  "channel_id"
                          <*> o .:  "ids"
                          <*> o .:? "guild_id"

-- | Structure containing information about all reactions that have been removed from a message
data ReactionRemoveAllInfo = ReactionRemoveAllInfo
  { reactionRemoveAllChannelId :: ChannelId -- ^ Channel in which the message is
  , reactionRemoveAllMessageId :: MessageId -- ^ The message the reactions were removed from
  , reactionRemoveAllGuildId   :: Maybe GuildId -- ^ Guild in which the message is (if any)
  } deriving (Show, Read, Eq, Ord)

instance FromJSON ReactionRemoveAllInfo where
  parseJSON = withObject "ReactionRemoveAllInfo" $ \o ->
    ReactionRemoveAllInfo <$> o .:  "channel_id"
                          <*> o .:  "message_id"
                          <*> o .:? "guild_id"

-- | Structure containing information about a created invite. Discord's @target_application@ is
-- not decoded, since the library has no type for an invite's partial application
data InviteCreateInfo = InviteCreateInfo
  { inviteCreateChannelId  :: ChannelId -- ^ Channel the invite is for
  , inviteCreateCode       :: T.Text -- ^ The invite code
  , inviteCreateCreatedAt  :: UTCTime -- ^ When the invite was created
  , inviteCreateGuildId    :: Maybe GuildId -- ^ Guild the invite is for
  , inviteCreateInviter    :: Maybe User -- ^ User that created the invite
  , inviteCreateMaxAge     :: Integer -- ^ Duration (in seconds) after which the invite expires, 0 for never
  , inviteCreateMaxUses    :: Integer -- ^ Maximum number of times the invite can be used, 0 for unlimited
  , inviteCreateTargetType :: Maybe Int -- ^ Type of target for a voice channel invite (1 for a stream, 2 for an embedded application)
  , inviteCreateTargetUser :: Maybe User -- ^ User whose stream to display for a voice channel stream invite
  , inviteCreateTemporary  :: Bool -- ^ Whether the invite only grants temporary membership
  , inviteCreateUses       :: Integer -- ^ How many times the invite has been used (always 0)
  , inviteCreateExpiresAt  :: Maybe UTCTime -- ^ When the invite expires, if it does
  , inviteCreateRoleIds    :: Maybe [RoleId] -- ^ Roles the invite assigns to the user who accepts it
  } deriving (Show, Read, Eq, Ord)

instance FromJSON InviteCreateInfo where
  parseJSON = withObject "InviteCreateInfo" $ \o ->
    InviteCreateInfo <$> o .:  "channel_id"
                     <*> o .:  "code"
                     <*> o .:  "created_at"
                     <*> o .:? "guild_id"
                     <*> o .:? "inviter"
                     <*> o .:  "max_age"
                     <*> o .:  "max_uses"
                     <*> o .:? "target_type"
                     <*> o .:? "target_user"
                     <*> o .:  "temporary"
                     <*> o .:  "uses"
                     <*> o .:? "expires_at"
                     <*> o .:? "role_ids"

-- | Structure containing information about a deleted invite
data InviteDeleteInfo = InviteDeleteInfo
  { inviteDeleteChannelId :: ChannelId -- ^ Channel the invite was for
  , inviteDeleteGuildId   :: Maybe GuildId -- ^ Guild the invite was for
  , inviteDeleteCode      :: T.Text -- ^ The invite code
  } deriving (Show, Read, Eq, Ord)

instance FromJSON InviteDeleteInfo where
  parseJSON = withObject "InviteDeleteInfo" $ \o ->
    InviteDeleteInfo <$> o .:  "channel_id"
                     <*> o .:? "guild_id"
                     <*> o .:  "code"

-- | Structure containing information about a reaction that has been removed
data ReactionRemoveInfo  = ReactionRemoveInfo
  { reactionRemoveChannelId :: ChannelId
  , reactionRemoveGuildId   :: GuildId
  , reactionRemoveMessageId :: MessageId
  , reactionRemoveEmoji     :: Emoji
  } deriving (Show, Read, Eq, Ord)

instance FromJSON ReactionRemoveInfo where
  parseJSON = withObject "ReactionRemoveInfo" $ \o ->
    ReactionRemoveInfo <$> o .:  "channel_id"
                       <*> o .:  "guild_id"
                       <*> o .:  "message_id"
                       <*> o .:  "emoji"

-- | Structure containing information about a user subscribing to or unsubscribing from a scheduled event
data ScheduledEventUserInfo = ScheduledEventUserInfo
  { scheduledEventUserEventId :: ScheduledEventId -- ^ The scheduled event the user subscribed to or unsubscribed from
  , scheduledEventUserUserId  :: UserId -- ^ The user who subscribed or unsubscribed
  , scheduledEventUserGuildId :: GuildId -- ^ Guild in which the scheduled event is
  } deriving (Show, Read, Eq, Ord)

instance FromJSON ScheduledEventUserInfo where
  parseJSON = withObject "ScheduledEventUserInfo" $ \o ->
    ScheduledEventUserInfo <$> o .: "guild_scheduled_event_id"
                           <*> o .: "user_id"
                           <*> o .: "guild_id"

-- | Structure containing information about a vote added to or removed from a poll
data PollVoteInfo = PollVoteInfo
  { pollVoteUserId    :: UserId -- ^ User who voted or removed their vote
  , pollVoteChannelId :: ChannelId -- ^ Channel in which the poll's message is
  , pollVoteMessageId :: MessageId -- ^ The poll's message
  , pollVoteGuildId   :: Maybe GuildId -- ^ Guild in which the poll's message is (if any)
  , pollVoteAnswerId  :: Int -- ^ Id of the answer the vote is for
  } deriving (Show, Read, Eq, Ord)

instance FromJSON PollVoteInfo where
  parseJSON = withObject "PollVoteInfo" $ \o ->
    PollVoteInfo <$> o .:  "user_id"
                 <*> o .:  "channel_id"
                 <*> o .:  "message_id"
                 <*> o .:? "guild_id"
                 <*> o .:  "answer_id"

-- | Structre containing typing status information
data TypingInfo = TypingInfo
  { typingUserId    :: UserId
  , typingChannelId :: ChannelId
  , typingTimestamp :: UTCTime
  } deriving (Show, Read, Eq, Ord)

instance FromJSON TypingInfo where
  parseJSON = withObject "TypingInfo" $ \o ->
    do cid <- o .: "channel_id"
       uid <- o .: "user_id"
       posix <- o .: "timestamp"
       let utc = posixSecondsToUTCTime posix
       pure (TypingInfo uid cid utc)

-- | Structure containing auto moderation action execution information
data AutoModerationActionExecuteInfo = AutoModerationActionExecuteInfo
  { autoModerationActionExecuteInfoGuildId        :: GuildId
  , autoModerationActionExecuteInfoAction         :: AutoModerationRuleAction
  , autoModerationActionExecuteInfoRuleId         :: AutoModerationRuleId
  , autoModerationActionExecuteInfoTriggerType    :: AutoModerationRuleTriggerType
  , autoModerationActionExecuteInfoUserId         :: UserId
  , autoModerationActionExecuteInfoChannelId      :: Maybe ChannelId
  , autoModerationActionExecuteInfoMessageId      :: Maybe MessageId
  , autoModerationActionExecuteInfoAlertMessageId :: Maybe MessageId
  , autoModerationActionExecuteInfoContent        :: String
  , autoModerationActionExecuteInfoMatchedKeyword :: Maybe String
  , autoModerationActionExecuteInfoMatchedContent :: Maybe String
  } deriving ( Eq, Show, Read )

instance FromJSON AutoModerationActionExecuteInfo where
  parseJSON = withObject "AutoModerationActionExecuteInfo" $ \o ->
    AutoModerationActionExecuteInfo
      <$> o .:  "guild_id"
      <*> o .:  "action"
      <*> o .:  "rule_id"
      <*> o .:  "rule_trigger_type"
      <*> o .:  "user_id"
      <*> o .:? "channel_id"
      <*> o .:? "message_id"
      <*> o .:? "alert_system_message_id"
      <*> o .:  "content"
      <*> o .:? "matched_keyword"
      <*> o .:? "matched_content"

instance ToJSON AutoModerationActionExecuteInfo where
  toJSON AutoModerationActionExecuteInfo{..} = objectFromMaybes
    [ "guild_id"                  .== autoModerationActionExecuteInfoGuildId
    , "action"                    .== autoModerationActionExecuteInfoAction
    , "rule_id"                   .== autoModerationActionExecuteInfoRuleId
    , "rule_trigger_type"         .== autoModerationActionExecuteInfoTriggerType
    , "user_id"                   .== autoModerationActionExecuteInfoUserId
    , "channel_id"                .=? autoModerationActionExecuteInfoChannelId
    , "message_id"                .=? autoModerationActionExecuteInfoMessageId
    , "alert_system_message_id"   .=? autoModerationActionExecuteInfoAlertMessageId
    , "content"                   .== autoModerationActionExecuteInfoContent
    , "matched_keyword"           .=? autoModerationActionExecuteInfoMatchedKeyword
    , "matched_content"           .=? autoModerationActionExecuteInfoMatchedContent
    ]

-- | Convert ToJSON value to FromJSON value
reparse :: (ToJSON a, FromJSON b) => a -> Parser b
reparse val = case parseEither parseJSON $ toJSON val of
                Left r -> fail r
                Right b -> pure b

-- | Remove the "wss://" and the trailing slash in a gateway URL, thereby returning
-- the hostname portion of the URL that we can connect to.
extractHostname :: String -> HostName
extractHostname ('w':'s':'s':':':'/':'/':rest) = extractHostname rest
extractHostname "/" = []
extractHostname (a:b) = a:extractHostname b
extractHostname [] = []

-- | Parse an event from name and JSON data
eventParse :: T.Text -> Object -> Parser EventInternalParse
eventParse t o = case t of
    "READY"                     -> InternalReady <$> o .: "v"
                                         <*> o .: "user"
                                         <*> o .: "guilds"
                                         <*> o .: "session_id"
                                          -- Discord can send us the resume gateway URL prefixed with "wss://",
                                          -- and suffixed with a trailing slash. This is not a valid HostName,
                                          -- so remove them both if they exist.
                                         <*> (extractHostname <$> o .: "resume_gateway_url")
                                         <*> o .: "shard"
                                         <*> o .: "application"
    "RESUMED"                   -> InternalResumed <$> o .: "_trace"
    "AUTO_MODERATION_RULE_CREATE" -> InternalAutoModerationRuleCreate <$> reparse o
    "AUTO_MODERATION_RULE_UPDATE" -> InternalAutoModerationRuleUpdate <$> reparse o
    "AUTO_MODERATION_RULE_DELETE" -> InternalAutoModerationRuleDelete <$> reparse o
    "AUTO_MODERATION_ACTION_EXECUTION" -> InternalAutoModerationActionExecution <$> reparse o
    "CHANNEL_CREATE"            -> InternalChannelCreate             <$> reparse o
    "CHANNEL_UPDATE"            -> InternalChannelUpdate             <$> reparse o
    "CHANNEL_DELETE"            -> InternalChannelDelete             <$> reparse o
    "THREAD_CREATE"             -> InternalThreadCreate              <$> reparse o
    "THREAD_UPDATE"             -> InternalThreadUpdate              <$> reparse o
    "THREAD_MEMBER_UPDATE"      -> InternalThreadMemberUpdate        <$> reparse o
    "THREAD_DELETE"             -> InternalThreadDelete              <$> reparse o
    "THREAD_LIST_SYNC"          -> InternalThreadListSync            <$> reparse o
    "THREAD_MEMBERS_UPDATE"     -> InternalThreadMembersUpdate       <$> reparse o
    "CHANNEL_PINS_UPDATE"       -> do id <- o .: "channel_id"
                                      stamp <- o .:? "last_pin_timestamp"
                                      let utc = stamp >>= parseISO8601
                                      pure (InternalChannelPinsUpdate id utc)
    "GUILD_CREATE"              -> parseGuildCreate o
    "GUILD_UPDATE"              -> InternalGuildUpdate               <$> reparse o
    "GUILD_DELETE"              -> InternalGuildDelete               <$> reparse o
    "GUILD_AUDIT_LOG_ENTRY_CREATE" -> InternalGuildAuditLogEntryCreate <$> o .: "guild_id" <*> reparse o
    "GUILD_BAN_ADD"             -> InternalGuildBanAdd    <$> o .: "guild_id" <*> o .: "user"
    "GUILD_BAN_REMOVE"          -> InternalGuildBanRemove <$> o .: "guild_id" <*> o .: "user"
    "GUILD_EMOJI_UPDATE"        -> InternalGuildEmojiUpdate <$> o .: "guild_id" <*> o .: "emojis"
    "GUILD_STICKERS_UPDATE"     -> InternalGuildStickersUpdate <$> o .: "guild_id" <*> o .: "stickers"
    "GUILD_INTEGRATIONS_UPDATE" -> InternalGuildIntegrationsUpdate   <$> o .: "guild_id"
    "GUILD_MEMBER_ADD"          -> InternalGuildMemberAdd <$> o .: "guild_id" <*> reparse o
    "GUILD_MEMBER_REMOVE"       -> InternalGuildMemberRemove <$> o .: "guild_id" <*> o .: "user"
    "GUILD_MEMBER_UPDATE"       -> InternalGuildMemberUpdate <$> o .: "guild_id"
                                                             <*> o .: "roles"
                                                             <*> o .: "user"
                                                             <*> o .:? "nick"
    "GUILD_MEMBERS_CHUNK"       -> InternalGuildMemberChunk <$> o .: "guild_id" <*> o .: "members"
    "GUILD_ROLE_CREATE"         -> InternalGuildRoleCreate  <$> o .: "guild_id" <*> o .: "role"
    "GUILD_ROLE_UPDATE"         -> InternalGuildRoleUpdate  <$> o .: "guild_id" <*> o .: "role"
    "GUILD_ROLE_DELETE"         -> InternalGuildRoleDelete  <$> o .: "guild_id" <*> o .: "role_id"
    "GUILD_SCHEDULED_EVENT_CREATE" -> InternalGuildScheduledEventCreate <$> reparse o
    "GUILD_SCHEDULED_EVENT_UPDATE" -> InternalGuildScheduledEventUpdate <$> reparse o
    "GUILD_SCHEDULED_EVENT_DELETE" -> InternalGuildScheduledEventDelete <$> reparse o
    "GUILD_SCHEDULED_EVENT_USER_ADD" -> InternalGuildScheduledEventUserAdd <$> reparse o
    "GUILD_SCHEDULED_EVENT_USER_REMOVE" -> InternalGuildScheduledEventUserRemove <$> reparse o
    "MESSAGE_CREATE"            -> InternalMessageCreate     <$> reparse o
    "MESSAGE_UPDATE"            -> InternalMessageUpdate     <$> reparse o
    "MESSAGE_DELETE"            -> InternalMessageDelete     <$> reparse o
    "MESSAGE_DELETE_BULK"       -> InternalMessageDeleteBulk <$> reparse o
    "MESSAGE_REACTION_ADD"      -> InternalMessageReactionAdd <$> reparse o
    "MESSAGE_REACTION_REMOVE"   -> InternalMessageReactionRemove <$> reparse o
    "MESSAGE_REACTION_REMOVE_ALL" -> InternalMessageReactionRemoveAll <$> reparse o
    "MESSAGE_REACTION_REMOVE_EMOJI" -> InternalMessageReactionRemoveEmoji <$> reparse o
    "INVITE_CREATE"             -> InternalInviteCreate              <$> reparse o
    "INVITE_DELETE"             -> InternalInviteDelete              <$> reparse o
    "MESSAGE_POLL_VOTE_ADD"     -> InternalMessagePollVoteAdd        <$> reparse o
    "MESSAGE_POLL_VOTE_REMOVE"  -> InternalMessagePollVoteRemove     <$> reparse o
    "PRESENCE_UPDATE"           -> InternalPresenceUpdate            <$> reparse o
    "TYPING_START"              -> InternalTypingStart               <$> reparse o
    "USER_UPDATE"               -> InternalUserUpdate                <$> reparse o
    "WEBHOOKS_UPDATE"           -> InternalWebhooksUpdate            <$> o .: "guild_id" <*> o .: "channel_id"
 -- "VOICE_STATE_UPDATE"        -> InternalVoiceStateUpdate          <$> reparse o
 -- "VOICE_SERVER_UPDATE"       -> InternalVoiceServerUpdate         <$> reparse o
    "INTERACTION_CREATE"        -> InternalInteractionCreate         <$> reparse o
    _other_event                -> InternalUnknownEvent t            <$> reparse o
