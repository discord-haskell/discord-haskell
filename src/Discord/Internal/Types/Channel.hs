{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveDataTypeable #-}

-- | Data structures pertaining to Discord Channels
module Discord.Internal.Types.Channel (
    Channel (..)
  , channelIsInGuild
  , Overwrite (..)
  , ThreadMetadata (..)
  , ThreadMember (..)
  , ThreadListSyncFields (..)
  , ThreadMembersUpdateFields (..)
  , Message (..)
  , AllowedMentions (..)
  , MessageReaction (..)
  , Attachment (..)
  , Nonce (..)
  , MessageReference (..)
  , MessageType (..)
  , MessageActivity (..)
  , MessageActivityType (..)
  , MessageFlag (..)
  , MessageFlags (..)
  , MessageInteraction (..)

  , ChannelTypeOption (..)
  ) where

import Control.Applicative (empty)
import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Default (Default, def)
import Data.Text (Text)
import Data.Time.Clock
import qualified Data.Text as T
import Data.Bits
import Data.Data (Data)

import Discord.Internal.Types.Prelude
import Discord.Internal.Types.User (User(..), GuildMember)
import Discord.Internal.Types.Embed
import Discord.Internal.Types.Components (ActionRow)
import Discord.Internal.Types.Emoji

-- | Guild channels represent an isolated set of users and messages in a Guild (Server)
data Channel
  -- | A text channel in a guild.
  = ChannelText
      { channelId          :: ChannelId         -- ^ The id of the channel (Will be equal to
                                                --   the guild if it's the "general" channel).
      , channelGuild       :: GuildId           -- ^ The id of the guild.
      , channelName        :: T.Text            -- ^ The name of the channel (2 - 1000 characters).
      , channelPosition    :: Integer           -- ^ The storing position of the channel.
      , channelPermissions :: [Overwrite]       -- ^ An array of permission 'Overwrite's
      , channelUserRateLimit :: Integer         -- ^ Seconds before a user can speak again
      , channelNSFW        :: Bool              -- ^ Is not-safe-for-work
      , channelTopic       :: T.Text            -- ^ The topic of the channel. (0 - 1024 chars).
      , channelLastMessage :: Maybe MessageId   -- ^ The id of the last message sent in the
                                                --   channel
      , channelParentId    :: Maybe ParentId    -- ^ The id of the parent channel (category)
      }
  -- | A news Channel in a guild.
  | ChannelNews
      { channelId          :: ChannelId       -- ^ The id of the channel
      , channelGuild       :: GuildId         -- ^ The id of the guild
      , channelName        :: T.Text          -- ^ The name of the channel (2 - 1000 characters)
      , channelPosition    :: Integer         -- ^ The position of the channel
      , channelPermissions :: [Overwrite]     -- ^ An array of permission 'Overrite's
      , channelNSFW        :: Bool            -- ^ Is not-safe-for-work
      , channelTopic       :: T.Text          -- ^ Topic of the channel (0 - 1024 characters)
      , channelLastMessage :: Maybe MessageId -- ^ The ID of the last message of the channel
      , channelParentId    :: Maybe ParentId  -- ^ The id of the parent channel (category)
      }
   -- | A store page channel in a guild
  | ChannelStorePage
      { channelId          :: ChannelId      -- ^ The id of the channel
      , channelGuild       :: GuildId        -- ^ The id of the guild
      , channelName        :: T.Text         -- ^ The name of the channel (2 - 1000 characters)
      , channelPosition    :: Integer        -- ^ The position of the channel
      , channelNSFW        :: Bool           -- ^ Is not-safe-for-work
      , channelPermissions :: [Overwrite]    -- ^ An array of permission 'Overrite's
      , channelParentId    :: Maybe ParentId -- ^ The id of the parrent channel (category)
      }
  -- | A voice channel in a guild.
  | ChannelVoice
      { channelId          :: ChannelId       -- ^ The id of the channel
      , channelGuild       :: GuildId         -- ^ The id of the guild
      , channelName        :: T.Text          -- ^ The name of the channel (2 - 1000) characters
      , channelPosition    :: Integer         -- ^ The position of the channel
      , channelPermissions :: [Overwrite]     -- ^ An array of permission 'Overrite's
      , channelNSFW        :: Bool            -- ^ Is not-safe-for-work
      , channelBitRate     :: Integer         -- ^ The bitrate (in bps) of the channel.
      , channelUserLimit   :: Integer         -- ^ The user limit of the voice channel.
      , channelParentId    :: Maybe ParentId  -- ^ The id of the parrent channel (category)
      }
  -- | DM Channels represent a one-to-one conversation between two users, outside the scope
  --   of guilds
  | ChannelDirectMessage
      { channelId          :: ChannelId       -- ^ The id of the channel
      , channelRecipients  :: [User]          -- ^ The 'User' object(s) of the DM recipient(s).
      , channelLastMessage :: Maybe MessageId -- ^ The last message sent to the channel
      }
  -- | Like a 'ChannelDirectMessage' but for more people
  | ChannelGroupDM
      { channelId          :: ChannelId       -- ^ The id of the channel
      , channelRecipients  :: [User]          -- ^ The 'User' object(s) of the DM recipent(s).
      , channelLastMessage :: Maybe MessageId -- ^ The last message sent to the channel
      }
  -- | A channel category
  | ChannelGuildCategory
      { channelId          :: ChannelId   -- ^ The id of the category
      , channelGuild       :: GuildId     -- ^ The id of the gild
      , channelName        :: T.Text      -- ^ The name of the category
      , channelPosition    :: Integer     -- ^ The position of the category
      , channelPermissions :: [Overwrite] -- ^ A list of permission 'Overrite's
      }
  -- | A stage channel
  | ChannelStage
      { channelId          :: ChannelId -- ^ The id of the channel
      , channelGuild       :: GuildId   -- ^ The id of the guild
      , channelStageId     :: StageId   -- ^ The id of the stage
      , channelStageTopic  :: Text      -- ^ The topic text
      }
  -- | A news Thread
  | ChannelNewsThread
      { channelId          :: ChannelId               -- ^ The id of the thread
      , channelGuild       :: GuildId                 -- ^ The id of the guild.
      , channelThreadName  :: Maybe T.Text            -- ^ The name of the channel (2 - 1000 characters).
      , channelUserRateLimitThread :: Maybe Integer   -- ^ Seconds before a user can speak again
      , channelLastMessage :: Maybe MessageId         -- ^ The id of the last message sent in the
                                                      --   channel
      , channelParentId    :: Maybe ParentId          -- ^ The id of the parent channel
      , channelThreadMetadata :: Maybe ThreadMetadata -- ^ Metadata about this thread
      , channelThreadMember :: Maybe ThreadMember     -- ^ Used to indicate if the user has joined the thread
      }
  -- | A thread anyone can join
  | ChannelPublicThread
      { channelId          :: ChannelId               -- ^ The id of the thread
      , channelGuild       :: GuildId                 -- ^ The id of the guild.
      , channelThreadName  :: Maybe T.Text            -- ^ The name of the channel (2 - 1000 characters).
      , channelUserRateLimitThread :: Maybe Integer   -- ^ Seconds before a user can speak again
      , channelLastMessage :: Maybe MessageId         -- ^ The id of the last message sent in the
                                                      --   channel
      , channelParentId    :: Maybe ParentId          -- ^ The id of the parent channel
      , channelThreadMetadata :: Maybe ThreadMetadata -- ^ Metadata about this thread
      , channelThreadMember :: Maybe ThreadMember     -- ^ Used to indicate if the user has joined the thread
      }
  -- | An on-invite thread
  | ChannelPrivateThread
      { channelId          :: ChannelId               -- ^ The id of the thread
      , channelGuild       :: GuildId                 -- ^ The id of the guild.
      , channelThreadName  :: Maybe T.Text            -- ^ The name of the channel (2 - 1000 characters).
      , channelUserRateLimitThread :: Maybe Integer   -- ^ Seconds before a user can speak again
      , channelLastMessage :: Maybe MessageId         -- ^ The id of the last message sent in the
                                                      --   channel
      , channelParentId    :: Maybe ParentId          -- ^ The id of the parent channel
      , channelThreadMetadata :: Maybe ThreadMetadata -- ^ Metadata about this thread
      , channelThreadMember :: Maybe ThreadMember     -- ^ Used to indicate if the user has joined the thread
      }
  -- | A channel of unknown type
  | ChannelUnknownType
      { channelId          :: ChannelId -- ^ The id of the channel
      , channelJSON        :: Text      -- ^ The library couldn't parse the channel type, here is the raw JSON
      } deriving (Show, Read, Eq, Ord)

instance FromJSON Channel where
  parseJSON = withObject "Channel" $ \o -> do
    type' <- (o .: "type") :: Parser Int
    case type' of
      0 ->
        ChannelText  <$> o .:  "id"
                     <*> o .:? "guild_id" .!= 0
                     <*> o .:  "name"
                     <*> o .:  "position"
                     <*> o .:  "permission_overwrites"
                     <*> o .:  "rate_limit_per_user"
                     <*> o .:? "nsfw" .!= False
                     <*> o .:? "topic" .!= ""
                     <*> o .:? "last_message_id"
                     <*> o .:? "parent_id"
      1 ->
        ChannelDirectMessage <$> o .:  "id"
                             <*> o .:  "recipients"
                             <*> o .:? "last_message_id"
      2 ->
        ChannelVoice <$> o .:  "id"
                     <*> o .:? "guild_id" .!= 0
                     <*> o .:  "name"
                     <*> o .:  "position"
                     <*> o .:  "permission_overwrites"
                     <*> o .:? "nsfw" .!= False
                     <*> o .:  "bitrate"
                     <*> o .:  "user_limit"
                     <*> o .:? "parent_id"
      3 ->
        ChannelGroupDM <$> o .:  "id"
                       <*> o .:  "recipients"
                       <*> o .:? "last_message_id"
      4 ->
        ChannelGuildCategory <$> o .: "id"
                             <*> o .:? "guild_id" .!= 0
                             <*> o .:  "name"
                             <*> o .:  "position"
                             <*> o .:  "permission_overwrites"
      5 ->
        ChannelNews <$> o .:  "id"
                    <*> o .:? "guild_id" .!= 0
                    <*> o .:  "name"
                    <*> o .:  "position"
                    <*> o .:  "permission_overwrites"
                    <*> o .:? "nsfw" .!= False
                    <*> o .:? "topic" .!= ""
                    <*> o .:? "last_message_id"
                    <*> o .:? "parent_id"
      6 ->
        ChannelStorePage <$> o .:  "id"
                         <*> o .:? "guild_id" .!= 0
                         <*> o .:  "name"
                         <*> o .:  "position"
                         <*> o .:? "nsfw" .!= False
                         <*> o .:  "permission_overwrites"
                         <*> o .:? "parent_id"
      10 -> ChannelNewsThread <$> o.: "id"
                              <*> o .:? "guild_id" .!= 0
                              <*> o .:? "name"
                              <*> o .:? "rate_limit_per_user"
                              <*> o .:? "last_message_id"
                              <*> o .:? "parent_id"
                              <*> o .:? "thread_metadata"
                              <*> o .:? "member"
      11 -> ChannelPublicThread <$> o.: "id"
                                <*> o .:? "guild_id" .!= 0
                                <*> o .:? "name"
                                <*> o .:? "rate_limit_per_user"
                                <*> o .:? "last_message_id"
                                <*> o .:? "parent_id"
                                <*> o .:? "thread_metadata"
                                <*> o .:? "member"
      12 -> ChannelPrivateThread <$> o.: "id"
                                 <*> o .:? "guild_id" .!= 0
                                 <*> o .:? "name"
                                 <*> o .:? "rate_limit_per_user"
                                 <*> o .:? "last_message_id"
                                 <*> o .:? "parent_id"
                                 <*> o .:? "thread_metadata"
                                 <*> o .:? "member"
      13 ->
        ChannelStage <$> o .:  "id"
                     <*> o .:? "guild_id" .!= 0
                     <*> o .:  "id"
                     <*> o .:? "topic" .!= ""
      _ -> ChannelUnknownType <$> o .:  "id"
                              <*> pure (T.pack (show o))

instance ToJSON Channel where
  toJSON ChannelText{..} = objectFromMaybes
              [ "type" .== Number 0
              , "id" .== channelId
              , "guild_id" .== channelGuild
              , "name" .== channelName
              , "position" .== channelPosition
              , "rate_limit_per_user" .== channelUserRateLimit
              , "nsfw" .== channelNSFW
              , "permission_overwrites" .== channelPermissions
              , "topic" .== channelTopic
              , "last_message_id" .=? channelLastMessage
              , "parent_id" .== channelParentId
              ]
  toJSON ChannelNews{..} = objectFromMaybes
              [ "type" .== Number 5
              , "id" .== channelId
              , "guild_id" .== channelGuild
              , "name" .== channelName
              , "position" .== channelPosition
              , "permission_overwrites" .== channelPermissions
              , "nsfw" .== channelNSFW
              , "topic" .== channelTopic
              , "last_message_id" .=? channelLastMessage
              , "parent_id" .=? channelParentId
              ]
  toJSON ChannelStorePage{..} = objectFromMaybes
              [ "type" .== Number 6
              , "id" .== channelId
              , "guild_id" .== channelGuild
              , "name" .== channelName
              , "nsfw" .== channelNSFW
              , "position" .== channelPosition
              , "permission_overwrites" .== channelPermissions
              ]
  toJSON ChannelDirectMessage{..} = objectFromMaybes
              [ "type" .== Number 1
              , "id" .== channelId
              , "recipients" .== channelRecipients
              , "last_message_id" .=? channelLastMessage
              ]
  toJSON ChannelVoice{..} = objectFromMaybes
              [ "type" .== Number 2
              , "id" .== channelId
              , "guild_id" .== channelGuild
              , "name" .== channelName
              , "position" .== channelPosition
              , "nsfw" .== channelNSFW
              , "permission_overwrites" .== channelPermissions
              , "bitrate" .== channelBitRate
              , "user_limit" .== channelUserLimit
              ]
  toJSON ChannelGroupDM{..} = objectFromMaybes
              [ "type" .== Number 3
              , "id" .== channelId
              , "recipients" .== channelRecipients
              , "last_message_id" .=? channelLastMessage
              ]
  toJSON ChannelGuildCategory{..} = objectFromMaybes
              [ "type" .== Number 4
              , "id" .== channelId
              , "name" .== channelName
              , "guild_id" .== channelGuild
              ]
  toJSON ChannelStage{..} = objectFromMaybes
              [ "type" .== Number 13
              , "id" .== channelId
              , "guild_id" .== channelGuild
              , "channel_id" .== channelStageId
              , "topic" .== channelStageTopic
              ]
  toJSON ChannelNewsThread{..} = objectFromMaybes
              [ "type" .== Number 10
              , "id" .== channelId
              , "guild_id" .== channelGuild
              , "name" .=? channelThreadName
              , "rate_limit_per_user" .=? channelUserRateLimitThread
              , "last_message_id" .=? channelLastMessage
              , "parent_id" .== channelParentId
              , "thread_metadata" .=? channelThreadMetadata
              , "member" .=? channelThreadMember
              ]
  toJSON ChannelPublicThread{..} = objectFromMaybes
              [ "type" .== Number 11
              , "id" .== channelId
              , "guild_id" .== channelGuild
              , "name" .=? channelThreadName
              , "rate_limit_per_user" .=? channelUserRateLimitThread
              , "last_message_id" .=? channelLastMessage
              , "parent_id" .== channelParentId
              , "thread_metadata" .=? channelThreadMetadata
              , "member" .=? channelThreadMember
              ]
  toJSON ChannelPrivateThread{..} = objectFromMaybes
              [ "type" .== Number 12
              , "id" .== channelId
              , "guild_id" .== channelGuild
              , "name" .=? channelThreadName
              , "rate_limit_per_user" .=? channelUserRateLimitThread
              , "last_message_id" .=? channelLastMessage
              , "parent_id" .== channelParentId
              , "thread_metadata" .=? channelThreadMetadata
              , "member" .=? channelThreadMember
              ]
  toJSON ChannelUnknownType{..} = objectFromMaybes
              [ "id" .== channelId
              , "json" .== channelJSON
              ]

-- | If the channel is part of a guild (has a guild id field)
channelIsInGuild :: Channel -> Bool
channelIsInGuild c = case c of
        ChannelGuildCategory{} -> True
        ChannelText{} -> True
        ChannelVoice{} -> True
        ChannelNews{} -> True
        ChannelStorePage{} -> True
        ChannelNewsThread{} -> True
        ChannelPublicThread{} -> True
        ChannelPrivateThread{} -> True
        _ -> False

-- | Permission overwrites for a channel.
data Overwrite = Overwrite
  { overwriteId    :: Either RoleId UserId -- ^ 'Role' or 'User' id
  , overwriteAllow :: T.Text               -- ^ Allowed permission bit set
  , overwriteDeny  :: T.Text               -- ^ Denied permission bit set
  } deriving (Show, Read, Eq, Ord)

instance FromJSON Overwrite where
  parseJSON = withObject "Overwrite" $ \o -> do
    t <- o .: "type"
    i <- case (t :: Int) of
      0 -> Left <$> o .: "id"
      1 -> Right <$> o .: "id"
      _ -> error "Type field can only be 0 (role id) or 1 (user id)"
    Overwrite i
              <$> o .: "allow"
              <*> o .: "deny"

instance ToJSON Overwrite where
  toJSON Overwrite{..} = object
              [ ("id",     toJSON $ either unId unId overwriteId)
              , ("type",   toJSON (either (const 0) (const 1) overwriteId :: Int))
              , ("allow",  toJSON overwriteAllow)
              , ("deny",   toJSON overwriteDeny)
              ]

-- | Metadata for threads.
data ThreadMetadata = ThreadMetadata
 { threadMetadataArchived :: Bool -- ^ Is the thread archived?
 , threadMetadataAutoArchive :: Integer -- ^ How long after activity should the thread auto archive
 , threadMetadataArchiveTime :: UTCTime -- ^ When was the last time the archive status changed?
 , threadMetadataLocked :: Bool -- ^ Is the thread locked? (only MANAGE_THREADS users can unarchive)
 , threadMetadataInvitable :: Maybe Bool -- ^ Can non-mods add other non-mods? (private threads only)
 , threadMetadataCreateTime :: Maybe UTCTime -- ^ When was the thread created?
 } deriving (Show, Read, Eq, Ord)

instance FromJSON ThreadMetadata where
  parseJSON = withObject "ThreadMetadata" $ \o ->
    ThreadMetadata <$> o .:  "archived"
                   <*> o .:  "auto_archive_duration"
                   <*> o .:  "archive_timestamp"
                   <*> o .:  "locked"
                   <*> o .:? "invitable"
                   <*> o .:? "create_timestamp"

instance ToJSON ThreadMetadata where
  toJSON ThreadMetadata{..} = objectFromMaybes
              [ "archived" .== threadMetadataArchived
              , "auto_archive_duration" .== threadMetadataAutoArchive
              , "archive_timestamp" .== threadMetadataArchiveTime
              , "locked" .== threadMetadataLocked
              , "invitable" .=? threadMetadataInvitable
              , "create_timestamp" .== threadMetadataCreateTime
              ]

-- | A user in a thread
data ThreadMember = ThreadMember
 { threadMemberThreadId :: Maybe ChannelId -- ^ id of the thread
 , threadMemberUserId   :: Maybe UserId    -- ^ id of the user
 , threadMemberJoinTime :: UTCTime         -- ^ time the current user last joined the thread
 , threadMemberFlags    :: Integer         -- ^ user-thread settings
 } deriving (Show, Read, Eq, Ord)

instance FromJSON ThreadMember where
  parseJSON = withObject "ThreadMember" $ \o ->
    ThreadMember <$> o .:? "id"
                 <*> o .:? "user_id"
                 <*> o .:  "join_timestamp"
                 <*> o .:  "flags"

instance ToJSON ThreadMember where
  toJSON ThreadMember{..} = objectFromMaybes
              [ "id" .=? threadMemberThreadId
              , "user_id" .=? threadMemberUserId
              , "join_timestamp" .== threadMemberJoinTime
              , "flags" .== threadMemberFlags
              ]


data ThreadListSyncFields = ThreadListSyncFields
  { threadListSyncFieldsGuildId :: GuildId
  , threadListSyncFieldsChannelIds :: Maybe [ChannelId]
  , threadListSyncFieldsThreads :: [Channel]
  , threadListSyncFieldsThreadMembers :: [ThreadMember]
  } deriving (Show, Read, Eq, Ord)

instance FromJSON ThreadListSyncFields where
  parseJSON = withObject "ThreadListSyncFields" $ \o ->
    ThreadListSyncFields <$> o .: "guild_id"
                         <*> o .:? "channel_ids"
                         <*> o .:  "threads"
                         <*> o .:  "members"

data ThreadMembersUpdateFields = ThreadMembersUpdateFields
  { threadMembersUpdateFieldsThreadId :: ChannelId
  , threadMembersUpdateFieldsGuildId :: GuildId
  , threadMembersUpdateFieldsMemberCount :: Integer
  , threadMembersUpdateFieldsAddedMembers :: Maybe [ThreadMember]
  , threadMembersUpdateFieldsRemovedMembers :: Maybe [UserId]
  } deriving (Show, Read, Eq, Ord)

instance FromJSON ThreadMembersUpdateFields where
  parseJSON = withObject "ThreadMembersUpdateFields" $ \o ->
    ThreadMembersUpdateFields <$> o .:  "id"
                              <*> o .:  "guild_id"
                              <*> o .:  "member_count"
                              <*> o .:? "added_members"
                              <*> o .:? "removed_member_ids"

-- | Represents information about a message in a Discord channel.
data Message = Message
  { messageId                 :: MessageId                -- ^ The id of the message
  , messageChannelId          :: ChannelId                -- ^ Id of the channel the message
                                                          --   was sent in
  , messageGuildId            :: Maybe GuildId            -- ^ The guild the message went to
  , messageAuthor             :: User                     -- ^ The 'User' the message was sent
                                                          --   by
  , messageMember             :: Maybe GuildMember        -- ^ A partial guild member object
  , messageContent            :: Text                     -- ^ Contents of the message
  , messageTimestamp          :: UTCTime                  -- ^ When the message was sent
  , messageEdited             :: Maybe UTCTime            -- ^ When/if the message was edited
  , messageTts                :: Bool                     -- ^ Whether this message was a TTS
                                                          --   message
  , messageEveryone           :: Bool                     -- ^ Whether this message mentions
                                                          --   everyone
  , messageMentions           :: [User]                   -- ^ 'User's specifically mentioned in
                                                          --   the message
  , messageMentionRoles       :: [RoleId]                 -- ^ 'Role's specifically mentioned in
                                                          --   the message
  , messageAttachments        :: [Attachment]             -- ^ Any attached files
  , messageEmbeds             :: [Embed]                  -- ^ Any embedded content
  , messageReactions          :: [MessageReaction]        -- ^ Any reactions to message
  , messageNonce              :: Maybe Nonce              -- ^ Used for validating if a message
                                                          --   was sent
  , messagePinned             :: Bool                     -- ^ Whether this message is pinned
  , messageWebhookId          :: Maybe WebhookId          -- ^ The webhook id of the webhook that made the message
  , messageType               :: MessageType              -- ^ What type of message is this.
  , messageActivity           :: Maybe MessageActivity    -- ^ sent with Rich Presence-related chat embeds
  , messageApplicationId      :: Maybe ApplicationId      -- ^ if the message is a response to an Interaction, this is the id of the interaction's application
  , messageReference          :: Maybe MessageReference   -- ^ Reference IDs of the original message
  , messageFlags              :: Maybe MessageFlags       -- ^ Various message flags
  , messageReferencedMessage  :: Maybe Message            -- ^ The full original message
  , messageInteraction        :: Maybe MessageInteraction -- ^ sent if message is an interaction response
  , messageThread             :: Maybe Channel            -- ^ the thread that was started from this message, includes thread member object
  , messageComponents         :: Maybe [ActionRow]        -- ^ sent if the message contains components like buttons, action rows, or other interactive components
  , messageStickerItems       :: Maybe [StickerItem]      -- ^ sent if the message contains stickers
  } deriving (Show, Read, Eq, Ord)

instance FromJSON Message where
  parseJSON = withObject "Message" $ \o ->
    Message <$> o .:  "id"
            <*> o .:  "channel_id"
            <*> o .:? "guild_id" .!= Nothing
            <*> (do isW <- o .:? "webhook_id"
                    a <- o .: "author"
                    case isW :: Maybe WebhookId of
                      Nothing -> pure a
                      Just _ -> pure $ a { userIsWebhook = True })
            <*> o .:? "member"
            <*> o .:? "content" .!= ""
            <*> o .:? "timestamp" .!= epochTime
            <*> o .:? "edited_timestamp"
            <*> o .:? "tts" .!= False
            <*> o .:? "mention_everyone" .!= False
            <*> o .:? "mentions" .!= []
            <*> o .:? "mention_roles" .!= []
            <*> o .:? "attachments" .!= []
            <*> o .:  "embeds"
            <*> o .:? "reactions" .!= []
            <*> o .:? "nonce"
            <*> o .:? "pinned" .!= False
            <*> o .:? "webhook_id"
            <*> o .:  "type"
            <*> o .:? "activity"
            -- <*> o .:? "application"
            <*> o .:? "application_id"
            <*> o .:? "message_reference" .!= Nothing
            <*> o .:? "flags"
            <*> o .:? "referenced_message" .!= Nothing
            <*> o .:? "interaction"
            <*> o .:? "thread"
            <*> o .:? "components"
            <*> o .:? "sticker_items"


instance ToJSON Message where
  toJSON Message {..} = objectFromMaybes
      [ "id" .== messageId
      , "channel_id" .== messageChannelId
      , "guild_id" .=? messageGuildId
      , "author" .== messageAuthor
      , "member" .=? messageMember
      , "content" .== messageContent
      , "timestamp" .== messageTimestamp
      , "edited_timestamp" .=? messageEdited
      , "tts" .== messageTts
      , "mention_everyone" .== messageEveryone
      , "mentions" .== messageMentions
      , "mention_roles" .== messageMentionRoles
      , "attachments" .== messageAttachments
      , "embeds" .== messageEmbeds
      , "reactions" .== messageReactions
      , "nonce" .=? messageNonce
      , "pinned" .== messagePinned
      , "webhook_id" .=? messageWebhookId
      , "type" .== messageType
      , "activity" .=? messageActivity
      -- , ("application",            toJSON <$>      messageApplication)
      , "application_id" .=? messageApplicationId
      , "message_reference" .=? messageReference
      , "flags" .=? messageFlags
      , "referenced_message" .=? messageReferencedMessage
      , "interaction" .=? messageInteraction
      , "thread" .=? messageThread
      , "components" .=? messageComponents
      , "sticker_items" .=? messageStickerItems
      ]

-- | Data constructor for a part of MessageDetailedOpts.
data AllowedMentions = AllowedMentions
  { mentionEveryone    :: Bool     -- ^ Can mention @\@everyone@
  , mentionUsers       :: Bool     -- ^ Can mention any user
  , mentionRoles       :: Bool     -- ^ Can mention any mentionable role
  , mentionUserIds     :: [UserId] -- ^ List of users able to be mentionned
  , mentionRoleIds     :: [RoleId] -- ^ List of roles able to be mentioneed 
  , mentionRepliedUser :: Bool     -- ^ Can mention the sender of the replied message 
  } deriving (Show, Read, Eq, Ord)

instance Default AllowedMentions where
  def = AllowedMentions { mentionEveryone    = False
                        , mentionUsers       = True
                        , mentionRoles       = True
                        , mentionUserIds     = []
                        , mentionRoleIds     = []
                        , mentionRepliedUser = True
                        }

instance ToJSON AllowedMentions where
  toJSON AllowedMentions{..} = object [
                                 "parse" .= [name :: T.Text | (name, True) <-
                                    [ ("everyone", mentionEveryone),
                                      ("users",    mentionUsers && null mentionUserIds),
                                      ("roles",    mentionRoles && null mentionRoleIds) ] ],
                                 -- https://discord.com/developers/docs/resources/channel#allowed-mentions-object
                                 --  parse.users and users list cannot both be active, prioritize id list
                                 "roles"        .= mentionRoleIds,
                                 "users"        .= mentionUserIds,
                                 "replied_user" .= mentionRepliedUser ]

-- | A reaction to a message
data MessageReaction = MessageReaction
  { messageReactionCount :: Int
  , messageReactionMeIncluded :: Bool
  , messageReactionEmoji :: Emoji
  } deriving (Show, Read, Eq, Ord)

instance FromJSON MessageReaction where
  parseJSON = withObject "MessageReaction" $ \o ->
    MessageReaction <$> o .: "count"
                    <*> o .: "me"
                    <*> o .: "emoji"

instance ToJSON MessageReaction where
  toJSON MessageReaction{..} = objectFromMaybes
      [ "count" .== messageReactionCount
      , "me" .== messageReactionMeIncluded
      , "emoji" .== messageReactionEmoji
      ]

-- | Represents an attached to a message file.
data Attachment = Attachment
  { attachmentId       :: AttachmentId     -- ^ Attachment id
  , attachmentFilename :: T.Text        -- ^ Name of attached file
  , attachmentSize     :: Integer       -- ^ Size of file (in bytes)
  , attachmentUrl      :: T.Text        -- ^ Source of file
  , attachmentProxy    :: T.Text        -- ^ Proxied url of file
  , attachmentHeight   :: Maybe Integer -- ^ Height of file (if image)
  , attachmentWidth    :: Maybe Integer -- ^ Width of file (if image)
  } deriving (Show, Read, Eq, Ord)

instance FromJSON Attachment where
  parseJSON = withObject "Attachment" $ \o ->
    Attachment <$> o .:  "id"
               <*> o .:  "filename"
               <*> o .:  "size"
               <*> o .:  "url"
               <*> o .:  "proxy_url"
               <*> o .:? "height"
               <*> o .:? "width"

instance ToJSON Attachment where
  toJSON Attachment {..} = objectFromMaybes
      [ "id" .== attachmentId
      , "filename" .== attachmentFilename
      , "size" .== attachmentSize
      , "url" .== attachmentUrl
      , "proxy_url" .== attachmentProxy
      , "height" .=? attachmentHeight
      , "width" .=? attachmentWidth
      ]

newtype Nonce = Nonce T.Text
  deriving (Show, Read, Eq, Ord)

instance FromJSON Nonce where
  parseJSON (String nonce) = pure $ Nonce nonce
  parseJSON (Number nonce) = pure . Nonce . T.pack . show $ nonce
  parseJSON _ = empty

instance ToJSON Nonce where
  toJSON (Nonce t) = String t


-- | Represents a Message Reference
data MessageReference = MessageReference
  { referenceMessageId      :: Maybe MessageId  -- ^ id of the originating message
  , referenceChannelId      :: Maybe ChannelId  -- ^ id of the originating message's channel
  , referenceGuildId        :: Maybe GuildId    -- ^ id of the originating message's guild
  , failIfNotExists         :: Bool             -- ^ Whether to not send if reference not exist
  } deriving (Show, Read, Eq, Ord)

instance FromJSON MessageReference where
  parseJSON = withObject "MessageReference" $ \o ->
    MessageReference <$> o .:? "message_id"
                     <*> o .:? "channel_id"
                     <*> o .:? "guild_id"
                     <*> o .:? "fail_if_not_exists" .!= True

instance ToJSON MessageReference where
  toJSON MessageReference{..} = objectFromMaybes
              [ "message_id" .== referenceMessageId
              , "channel_id" .== referenceChannelId
              , "guild_id" .== referenceGuildId
              , "fail_if_not_exists" .== failIfNotExists
              ]

instance Default MessageReference where
  def = MessageReference { referenceMessageId = Nothing
                         , referenceChannelId = Nothing
                         , referenceGuildId   = Nothing
                         , failIfNotExists    = False
                         }


data MessageType
  = MessageTypeDefault
  | MessageTypeRecipientAdd
  | MessageTypeRecipientRemove
  | MessageTypeCall
  | MessageTypeChannelNameChange
  | MessageTypeChannelIconChange
  | MessageTypeChannelPinnedMessage
  | MessageTypeGuildMemberJoin
  | MessageTypeUserPremiumGuildSubscription
  | MessageTypeUserPremiumGuildSubscriptionTier1
  | MessageTypeUserPremiumGuildSubscriptionTier2
  | MessageTypeUserPremiumGuildSubscriptionTier3
  | MessageTypeChannelFollowAdd
  | MessageTypeGuildDiscoveryDisqualified
  | MessageTypeGuildDiscoveryRequalified
  | MessageTypeGuildDiscoveryGracePeriodInitialWarning
  | MessageTypeGuildDiscoveryGracePeriodFinalWarning
  | MessageTypeThreadCreated
  | MessageTypeReply
  | MessageTypeChatInputCommand
  | MessageTypeThreadStarterMessage
  | MessageTypeGuildInviteReminder
  | MessageTypeContextMenuCommand
  deriving (Show, Read, Data, Eq, Ord)

instance InternalDiscordEnum MessageType where
  discordTypeStartValue = MessageTypeDefault
  fromDiscordType MessageTypeDefault = 0
  fromDiscordType MessageTypeRecipientAdd = 1
  fromDiscordType MessageTypeRecipientRemove = 2
  fromDiscordType MessageTypeCall = 3
  fromDiscordType MessageTypeChannelNameChange = 4
  fromDiscordType MessageTypeChannelIconChange = 5
  fromDiscordType MessageTypeChannelPinnedMessage = 6
  fromDiscordType MessageTypeGuildMemberJoin = 7
  fromDiscordType MessageTypeUserPremiumGuildSubscription = 8
  fromDiscordType MessageTypeUserPremiumGuildSubscriptionTier1 = 9
  fromDiscordType MessageTypeUserPremiumGuildSubscriptionTier2 = 10
  fromDiscordType MessageTypeUserPremiumGuildSubscriptionTier3 = 11
  fromDiscordType MessageTypeChannelFollowAdd = 12
  fromDiscordType MessageTypeGuildDiscoveryDisqualified = 14
  fromDiscordType MessageTypeGuildDiscoveryRequalified = 15
  fromDiscordType MessageTypeGuildDiscoveryGracePeriodInitialWarning = 16
  fromDiscordType MessageTypeGuildDiscoveryGracePeriodFinalWarning = 17
  fromDiscordType MessageTypeThreadCreated = 18
  fromDiscordType MessageTypeReply = 19
  fromDiscordType MessageTypeChatInputCommand = 20
  fromDiscordType MessageTypeThreadStarterMessage = 21
  fromDiscordType MessageTypeGuildInviteReminder = 22
  fromDiscordType MessageTypeContextMenuCommand = 23

instance ToJSON MessageType where
  toJSON = toJSON . fromDiscordType

instance FromJSON MessageType where
  parseJSON = discordTypeParseJSON "MessageType"

data MessageActivity = MessageActivity
  { messageActivityType :: MessageActivityType
  , messageActivityPartyId :: Maybe T.Text
  }
  deriving (Show, Read, Data, Eq, Ord)

instance FromJSON MessageActivity where
  parseJSON = withObject "MessageActivity" $ \o ->
    MessageActivity <$> o .:   "type"
                     <*> o .:? "party_id"

instance ToJSON MessageActivity where
  toJSON MessageActivity{..} = objectFromMaybes
              [ "type" .== messageActivityType
              , "party_id" .=? messageActivityPartyId
              ]

data MessageActivityType
  = MessageActivityTypeJoin -- ^ Join a Rich Presence event
  | MessageActivityTypeSpectate -- ^ Spectate a Rich Presence event
  | MessageActivityTypeListen -- ^ Listen to a Rich Presence event
  | MessageActivityTypeJoinRequest -- ^ Request to join a Rich Presence event
  deriving (Show, Read, Data, Eq, Ord)

instance InternalDiscordEnum MessageActivityType where
  discordTypeStartValue = MessageActivityTypeJoin
  fromDiscordType MessageActivityTypeJoin = 1
  fromDiscordType MessageActivityTypeSpectate = 2
  fromDiscordType MessageActivityTypeListen = 3
  fromDiscordType MessageActivityTypeJoinRequest = 4

instance ToJSON MessageActivityType where
  toJSON = toJSON . fromDiscordType

instance FromJSON MessageActivityType where
  parseJSON = discordTypeParseJSON "MessageActivityType"

-- | Types of flags to attach to the message.
data MessageFlag =
    MessageFlagCrossposted
  | MessageFlagIsCrosspost
  | MessageFlagSupressEmbeds
  | MessageFlagSourceMessageDeleted
  | MessageFlagUrgent
  | MessageFlagHasThread
  | MessageFlagEphemeral
  | MessageFlagLoading
  | MessageFlagFailedToMentionRollesInThread
  deriving (Show, Read, Eq, Data, Ord)

newtype MessageFlags = MessageFlags [MessageFlag]
  deriving (Show, Read, Eq, Ord)

instance InternalDiscordEnum MessageFlag where
  discordTypeStartValue = MessageFlagCrossposted
  fromDiscordType MessageFlagCrossposted = 1 `shift` 0
  fromDiscordType MessageFlagIsCrosspost = 1 `shift` 1
  fromDiscordType MessageFlagSupressEmbeds = 1 `shift` 2
  fromDiscordType MessageFlagSourceMessageDeleted = 1 `shift` 3
  fromDiscordType MessageFlagUrgent = 1 `shift` 4
  fromDiscordType MessageFlagHasThread = 1 `shift` 5
  fromDiscordType MessageFlagEphemeral = 1 `shift` 6
  fromDiscordType MessageFlagLoading = 1 `shift` 7
  fromDiscordType MessageFlagFailedToMentionRollesInThread = 1 `shift` 8

instance ToJSON MessageFlags where
  toJSON (MessageFlags fs) = Number $ fromInteger $ fromIntegral $ foldr (.|.) 0 (fromDiscordType <$> fs)

-- TODO: maybe make this a type class or something - the ability to handle flags automatically would be Very Good.

instance FromJSON MessageFlags where
  parseJSON = withScientific "MessageFlags" $ \s ->
      let i = round s
          -- TODO check to see that we know about all the flags
          -- if i /= (i .&. range)
          -- range = sum $ fst <$> (discordTypeTable @MessageFlag)
      in return $ MessageFlags (snd <$> filter (\(i',_) -> i .&. i' == i') discordTypeTable)

-- | This is sent on the message object when the message is a response to an Interaction without an existing message (i.e., any non-component interaction).
data MessageInteraction = MessageInteraction
  { messageInteractionId :: InteractionId -- ^ Id of the interaction
  , messageInteractionType :: Integer -- ^ Type of the interaction (liekly always application command)
  , messageInteractionName :: T.Text -- ^ Name of the interaction
  , messageInteractionUser :: User -- ^ User who invoked the interaction
  } deriving (Show, Read, Eq, Ord)

instance ToJSON MessageInteraction where
  toJSON MessageInteraction{..} = objectFromMaybes
              [ "id"   .== messageInteractionId
              , "type" .== messageInteractionType
              , "name" .== messageInteractionName
              , "user" .== messageInteractionUser
              ]

instance FromJSON MessageInteraction where
  parseJSON = withObject "MessageInteraction" $ \o ->
    MessageInteraction <$> o .: "id"
                       <*> o .: "type"
                       <*> o .: "name"
                       <*> o .: "user"
