{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveDataTypeable #-}

-- | Data structures pertaining to Discord Channels
module Discord.Internal.Types.Channel where

import Control.Applicative (empty)
import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Default (Default, def)
import Data.Text (Text)
import Data.Time.Clock
import qualified Data.Text as T

import Discord.Internal.Types.Prelude
import Discord.Internal.Types.User (User(..), GuildMember)
import Discord.Internal.Types.Embed
import Data.Data (Data)
import Data.Maybe (fromJust)
import Data.Bits
import Discord.Internal.Types.Components (Component, Emoji)

-- | Guild channels represent an isolated set of users and messages in a Guild (Server)
data Channel
  -- | A text channel in a guild.
  = ChannelText
      { channelId          :: ChannelId   -- ^ The id of the channel (Will be equal to
                                          --   the guild if it's the "general" channel).
      , channelGuild       :: GuildId     -- ^ The id of the guild.
      , channelName        :: T.Text      -- ^ The name of the guild (2 - 1000 characters).
      , channelPosition    :: Integer     -- ^ The storing position of the channel.
      , channelPermissions :: [Overwrite] -- ^ An array of permission 'Overwrite's
      , channelUserRateLimit :: Integer   -- ^ Seconds before a user can speak again
      , channelNSFW        :: Bool        -- ^ Is not-safe-for-work
      , channelTopic       :: T.Text      -- ^ The topic of the channel. (0 - 1024 chars).
      , channelLastMessage :: Maybe MessageId   -- ^ The id of the last message sent in the
                                                --   channel
      , channelParentId    :: Maybe ParentId    -- ^ The id of the parent channel (category)
      }
  | ChannelNews
      { channelId          :: ChannelId
      , channelGuild       :: GuildId
      , channelName        :: T.Text
      , channelPosition    :: Integer
      , channelPermissions :: [Overwrite]
      , channelNSFW        :: Bool
      , channelTopic       :: T.Text
      , channelLastMessage :: Maybe MessageId
      }
  | ChannelStorePage
      { channelId          :: ChannelId
      , channelGuild       :: GuildId
      , channelName        :: T.Text
      , channelPosition    :: Integer
      , channelNSFW        :: Bool
      , channelPermissions :: [Overwrite]
      , channelParentId    :: Maybe ParentId
      }
  -- | A voice channel in a guild.
  | ChannelVoice
      { channelId          :: ChannelId
      , channelGuild       :: GuildId
      , channelName        :: T.Text
      , channelPosition    :: Integer
      , channelPermissions :: [Overwrite]
      , channelNSFW        :: Bool
      , channelBitRate     :: Integer     -- ^ The bitrate (in bits) of the channel.
      , channelUserLimit   :: Integer     -- ^ The user limit of the voice channel.
      , channelParentId    :: Maybe ParentId
      }
  -- | DM Channels represent a one-to-one conversation between two users, outside the scope
  --   of guilds
  | ChannelDirectMessage
      { channelId          :: ChannelId
      , channelRecipients  :: [User]      -- ^ The 'User' object(s) of the DM recipient(s).
      , channelLastMessage :: Maybe MessageId
      }
  | ChannelGroupDM
      { channelId          :: ChannelId
      , channelRecipients  :: [User]
      , channelLastMessage :: Maybe MessageId
      }
  | ChannelGuildCategory
      { channelId          :: ChannelId
      , channelGuild       :: GuildId
      , channelName        :: T.Text
      , channelPosition    :: Integer
      , channelPermissions :: [Overwrite]
      }
  | ChannelStage
      { channelId          :: ChannelId
      , channelGuild       :: GuildId
      , channelStageId     :: StageId
      , channelStageTopic  :: Text
      }
  | ChannelUnknownType
      { channelId          :: ChannelId
      , channelJSON        :: Text
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
      6 ->
        ChannelStorePage <$> o .:  "id"
                         <*> o .:? "guild_id" .!= 0
                         <*> o .:  "name"
                         <*> o .:  "position"
                         <*> o .:? "nsfw" .!= False
                         <*> o .:  "permission_overwrites"
                         <*> o .:? "parent_id"
      13 ->
        ChannelStage <$> o .:  "id"
                     <*> o .:? "guild_id" .!= 0
                     <*> o .:  "id"
                     <*> o .:? "topic" .!= ""
      _ -> ChannelUnknownType <$> o .:  "id"
                              <*> pure (T.pack (show o))

instance ToJSON Channel where
  toJSON ChannelText{..} = object [(name,value) | (name, Just value) <-
              [ ("id",     toJSON <$> pure channelId)
              , ("guild_id", toJSON <$> pure channelGuild)
              , ("name",  toJSON <$> pure channelName)
              , ("position",   toJSON <$> pure channelPosition)
              , ("rate_limit_per_user", toJSON <$> pure channelUserRateLimit)
              , ("nsfw", toJSON <$> pure channelNSFW)
              , ("permission_overwrites",   toJSON <$> pure channelPermissions)
              , ("topic",   toJSON <$> pure channelTopic)
              , ("last_message_id",  toJSON <$> channelLastMessage)
              , ("parent_id",  toJSON <$> pure channelParentId)
              ] ]
  toJSON ChannelNews{..} = object [(name,value) | (name, Just value) <-
              [ ("id",     toJSON <$> pure channelId)
              , ("guild_id", toJSON <$> pure channelGuild)
              , ("name",  toJSON <$> pure channelName)
              , ("position",   toJSON <$> pure channelPosition)
              , ("permission_overwrites",   toJSON <$> pure channelPermissions)
              , ("nsfw", toJSON <$> pure channelNSFW)
              , ("topic",   toJSON <$> pure channelTopic)
              , ("last_message_id",  toJSON <$> channelLastMessage)
              ] ]
  toJSON ChannelStorePage{..} = object [(name,value) | (name, Just value) <-
              [ ("id",     toJSON <$> pure channelId)
              , ("guild_id", toJSON <$> pure channelGuild)
              , ("name",  toJSON <$> pure channelName)
              , ("nsfw", toJSON <$> pure channelNSFW)
              , ("position",   toJSON <$> pure channelPosition)
              , ("permission_overwrites",   toJSON <$> pure channelPermissions)
              ] ]
  toJSON ChannelDirectMessage{..} = object [(name,value) | (name, Just value) <-
              [ ("id",     toJSON <$> pure channelId)
              , ("recipients",   toJSON <$> pure channelRecipients)
              , ("last_message_id",  toJSON <$> channelLastMessage)
              ] ]
  toJSON ChannelVoice{..} = object [(name,value) | (name, Just value) <-
              [ ("id",     toJSON <$> pure channelId)
              , ("guild_id", toJSON <$> pure channelGuild)
              , ("name",  toJSON <$> pure channelName)
              , ("position",   toJSON <$> pure channelPosition)
              , ("nsfw", toJSON <$> pure channelNSFW)
              , ("permission_overwrites",   toJSON <$> pure channelPermissions)
              , ("bitrate",   toJSON <$> pure channelBitRate)
              , ("user_limit",  toJSON <$> pure channelUserLimit)
              ] ]
  toJSON ChannelGroupDM{..} = object [(name,value) | (name, Just value) <-
              [ ("id",     toJSON <$> pure channelId)
              , ("recipients",   toJSON <$> pure channelRecipients)
              , ("last_message_id",  toJSON <$> channelLastMessage)
              ] ]
  toJSON ChannelGuildCategory{..} = object [(name,value) | (name, Just value) <-
              [ ("id",     toJSON <$> pure channelId)
              , ("name", toJSON <$> pure channelName)
              , ("guild_id", toJSON <$> pure channelGuild)
              ] ]
  toJSON ChannelStage{..} = object [(name,value) | (name, Just value) <-
              [ ("id",     toJSON <$> pure channelId)
              , ("guild_id", toJSON <$> pure channelGuild)
              , ("channel_id", toJSON <$> pure channelStageId)
              , ("topic", toJSON <$> pure channelStageTopic)
              ] ]
  toJSON ChannelUnknownType{..} = object [(name,value) | (name, Just value) <-
              [ ("id",     toJSON <$> pure channelId)
              , ("json", toJSON <$> pure channelJSON)
              ] ]

-- | If the channel is part of a guild (has a guild id field)
channelIsInGuild :: Channel -> Bool
channelIsInGuild c = case c of
        ChannelGuildCategory{..} -> True
        ChannelText{..} -> True
        ChannelVoice{..}  -> True
        ChannelNews{..}  -> True
        ChannelStorePage{..}  -> True
        _ -> False

-- | Permission overwrites for a channel.
data Overwrite = Overwrite
  { overwriteId    :: OverwriteId -- ^ 'Role' or 'User' id
  , overwriteType  :: Integer    -- ^ Either role (0) or member (1)
  , overwriteAllow :: T.Text   -- ^ Allowed permission bit set
  , overwriteDeny  :: T.Text   -- ^ Denied permission bit set
  } deriving (Show, Read, Eq, Ord)

instance FromJSON Overwrite where
  parseJSON = withObject "Overwrite" $ \o ->
    Overwrite <$> o .: "id"
              <*> o .: "type"
              <*> o .: "allow"
              <*> o .: "deny"

instance ToJSON Overwrite where
  toJSON Overwrite{..} = object
              [ ("id",     toJSON overwriteId)
              , ("type",   toJSON overwriteType)
              , ("allow",  toJSON overwriteAllow)
              , ("deny",   toJSON overwriteDeny)
              ]

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
  -- , messageApplication  :: Maybe ??? -- ^ a partial application object
  , messageApplicationId      :: Maybe ApplicationId      -- ^ if the message is a response to an Interaction, this is the id of the interaction's application
  , messageReference          :: Maybe MessageReference   -- ^ Reference IDs of the original message
  , messageFlags              :: Maybe MessageFlags       -- ^ Various message flags
  , messageReferencedMessage  :: Maybe Message            -- ^ The full original message
  , messageInteraction        :: Maybe MessageInteraction -- ^ sent if message is an interaction response
  , messageThread             :: Maybe Channel            -- ^ the thread that was started from this message, includes thread member object
  , messageComponents         :: Maybe [Component]        -- ^ sent if the message contains components like buttons, action rows, or other interactive components
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
  toJSON Message {..} = object [(name, value) | (name, Just value) <-
      [ ("id",                  toJSON <$> pure messageId)
      , ("channel_id",          toJSON <$> pure messageChannelId)
      , ("guild_id",            toJSON <$>      messageGuildId)
      , ("author",              toJSON <$> pure messageAuthor)
      , ("member",              toJSON <$>      messageMember)
      , ("content",             toJSON <$> pure messageContent)
      , ("timestamp",           toJSON <$> pure messageTimestamp)
      , ("edited_timestamp",    toJSON <$>      messageEdited)
      , ("tts",                 toJSON <$> pure messageTts)
      , ("mention_everyone",    toJSON <$> pure messageEveryone)
      , ("mentions",            toJSON <$> pure messageMentions)
      , ("mention_roles",       toJSON <$> pure messageMentionRoles)
      , ("attachments",         toJSON <$> pure messageAttachments)
      , ("embeds",              toJSON <$> pure messageEmbeds)
      , ("reactions",           toJSON <$> pure messageReactions)
      , ("nonce",               toJSON <$>      messageNonce)
      , ("pinned",              toJSON <$> pure messagePinned)
      , ("webhook_id",          toJSON <$>      messageWebhookId)
      , ("type",                toJSON <$> pure messageType)
      , ("activity",            toJSON <$>      messageActivity)
      -- , ("application",            toJSON <$>      messageApplication)
      , ("application_id",      toJSON <$>      messageApplicationId)
      , ("message_reference",   toJSON <$>      messageReference)
      , ("flags",               toJSON <$>      messageFlags)
      , ("referenced_message",  toJSON <$>      messageReferencedMessage)
      , ("interaction",         toJSON <$>      messageInteraction)
      , ("thread",              toJSON <$>      messageThread)
      , ("components",          toJSON <$>      messageComponents)
      , ("sticker_items",       toJSON <$>      messageStickerItems)
      ] ]

-- | Data constructor for a part of MessageDetailedOpts.
data AllowedMentions = AllowedMentions
  { mentionEveryone    :: Bool
  , mentionUsers       :: Bool
  , mentionRoles       :: Bool
  , mentionUserIds     :: [UserId]
  , mentionRoleIds     :: [RoleId]
  , mentionRepliedUser :: Bool
  } deriving (Show, Eq, Ord, Read)

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
                                 ("parse" .= [name :: T.Text | (name, True) <-
                                     [ ("everyone", mentionEveryone),
                                       ("users",    mentionUsers && mentionUserIds == []),
                                       ("roles",    mentionRoles && mentionRoleIds == []) ] ]),
                                 -- https://discord.com/developers/docs/resources/channel#allowed-mentions-object
                                 --  parse.users and users list cannot both be active, prioritize id list
                                 ("roles"        .= mentionRoleIds),
                                 ("users"        .= mentionUserIds),
                                 ("replied_user" .= mentionRepliedUser) ]

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
  toJSON MessageReaction{..} = object [(name, value) | (name, Just value) <-
      [ ("count", toJSON <$> pure messageReactionCount)
      , ("me",    toJSON <$> pure messageReactionMeIncluded)
      , ("emoji", toJSON <$> pure messageReactionEmoji)
      ]]

-- TODO: expand stickers and have full objects
data StickerItem = StickerItem
  { stickerItemId :: StickerId
  , stickerItemName :: T.Text
  , stickerItemFormatType :: StickerFormatType
  }  deriving (Show, Read, Eq, Ord)

instance FromJSON StickerItem where
  parseJSON = withObject "StickerItem" $ \o ->
    StickerItem <$> o .: "id"
                <*> o .: "name"
                <*> o .: "format_type"

instance ToJSON StickerItem where
  toJSON StickerItem{..} = object [(name, value) | (name, Just value) <-
      [ ("id",          toJSON <$> pure stickerItemId)
      , ("name",        toJSON <$> pure stickerItemName)
      , ("format_type", toJSON <$> pure stickerItemFormatType)
      ]]

data StickerFormatType =
    StickerFormatTypePNG
  | StickerFormatTypeAPNG
  | StickerFormatTypeLOTTIE
  deriving (Show, Read, Eq, Ord, Data)

instance Enum StickerFormatType where
  fromEnum StickerFormatTypePNG = 1
  fromEnum StickerFormatTypeAPNG = 2
  fromEnum StickerFormatTypeLOTTIE = 3
  toEnum a = fromJust $ lookup a table
    where
      table = makeTable StickerFormatTypePNG

instance ToJSON StickerFormatType where
  toJSON = toJSON . fromEnum

instance FromJSON StickerFormatType where
  parseJSON = withScientific "StickerFormatType" (return . toEnum . round)

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
  toJSON Attachment {..} = object [(name, value) | (name, Just value) <-
      [ ("id",        toJSON <$> pure attachmentId)
      , ("filename",  toJSON <$> pure attachmentFilename)
      , ("size",      toJSON <$> pure attachmentSize)
      , ("url",       toJSON <$> pure attachmentUrl)
      , ("proxy_url", toJSON <$> pure attachmentProxy)
      , ("height",    toJSON <$>      attachmentHeight)
      , ("width",     toJSON <$>      attachmentWidth)
      ] ]

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
  toJSON MessageReference{..} = object [(name,value) | (name, Just value) <-
              [ ("message_id",     toJSON <$> pure referenceMessageId)
              , ("channel_id", toJSON <$> pure referenceChannelId)
              , ("guild_id",  toJSON <$> pure referenceGuildId)
              , ("fail_if_not_exists",   toJSON <$> pure failIfNotExists)
              ] ]

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

instance Enum MessageType where
  fromEnum MessageTypeDefault = 0
  fromEnum MessageTypeRecipientAdd = 1
  fromEnum MessageTypeRecipientRemove = 2
  fromEnum MessageTypeCall = 3
  fromEnum MessageTypeChannelNameChange = 4
  fromEnum MessageTypeChannelIconChange = 5
  fromEnum MessageTypeChannelPinnedMessage = 6
  fromEnum MessageTypeGuildMemberJoin = 7
  fromEnum MessageTypeUserPremiumGuildSubscription = 8
  fromEnum MessageTypeUserPremiumGuildSubscriptionTier1 = 9
  fromEnum MessageTypeUserPremiumGuildSubscriptionTier2 = 10
  fromEnum MessageTypeUserPremiumGuildSubscriptionTier3 = 11
  fromEnum MessageTypeChannelFollowAdd = 12
  fromEnum MessageTypeGuildDiscoveryDisqualified = 14
  fromEnum MessageTypeGuildDiscoveryRequalified = 15
  fromEnum MessageTypeGuildDiscoveryGracePeriodInitialWarning = 16
  fromEnum MessageTypeGuildDiscoveryGracePeriodFinalWarning = 17
  fromEnum MessageTypeThreadCreated = 18
  fromEnum MessageTypeReply = 19
  fromEnum MessageTypeChatInputCommand = 20
  fromEnum MessageTypeThreadStarterMessage = 21
  fromEnum MessageTypeGuildInviteReminder = 22
  fromEnum MessageTypeContextMenuCommand = 23
  toEnum a = fromJust $ lookup a table
    where
      table = makeTable MessageTypeDefault

instance ToJSON MessageType where
  toJSON = toJSON . fromEnum

instance FromJSON MessageType where
  parseJSON = withScientific "MessageType" (return . toEnum . round)

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
  toJSON MessageActivity{..} = object [(name,value) | (name, Just value) <-
              [ ("type",     toJSON <$> pure messageActivityType)
              , ("party_id", toJSON <$>      messageActivityPartyId)
              ] ]

data MessageActivityType
  = MessageActivityTypeJoin -- ^ Join a Rich Presence event
  | MessageActivityTypeSpectate -- ^ Spectate a Rich Presence event
  | MessageActivityTypeListen -- ^ Listen to a Rich Presence event
  | MessageActivityTypeJoinRequest -- ^ Request to join a Rich Presence event
  deriving (Show, Read, Data, Eq, Ord)

instance Enum MessageActivityType where
  fromEnum MessageActivityTypeJoin = 1
  fromEnum MessageActivityTypeSpectate = 2
  fromEnum MessageActivityTypeListen = 3
  fromEnum MessageActivityTypeJoinRequest = 4
  toEnum a = fromJust $ lookup a table
    where
      table = makeTable MessageActivityTypeJoin

instance ToJSON MessageActivityType where
  toJSON = toJSON . fromEnum

instance FromJSON MessageActivityType where
  parseJSON = withScientific "MessageActivityType" (return . toEnum . round)

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
  deriving (Show, Read, Eq, Data, Ord)

newtype MessageFlags = MessageFlags [MessageFlag]
  deriving (Show, Read, Eq, Ord)

instance Enum MessageFlag where
  fromEnum MessageFlagCrossposted = 1 `shift` 0
  fromEnum MessageFlagIsCrosspost = 1 `shift` 1
  fromEnum MessageFlagSupressEmbeds = 1 `shift` 2
  fromEnum MessageFlagSourceMessageDeleted = 1 `shift` 3
  fromEnum MessageFlagUrgent = 1 `shift` 4
  fromEnum MessageFlagHasThread = 1 `shift` 5
  fromEnum MessageFlagEphemeral = 1 `shift` 6
  fromEnum MessageFlagLoading = 1 `shift` 7
  toEnum a = fromJust $ lookup a table
    where
      table = makeTable MessageFlagCrossposted

instance ToJSON MessageFlags where
  toJSON (MessageFlags fs) = Number $ fromInteger $ fromIntegral $ foldr (.|.) 0 (fromEnum <$> fs)

-- TODO: maybe make this a type class or something - the ability to handle flags automatically would be Very Good.

instance FromJSON MessageFlags where
  parseJSON = withScientific "MessageFlags" (\s -> let i = round s in if i /= (i .&. range) then fail "could not get message flags" else return $ MessageFlags (snd <$> filter (\(i',_) -> i .&. i' == i') table))
    where 
      table = makeTable MessageFlagCrossposted
      range = sum $ fst <$> table

data MessageInteraction = MessageInteraction
  { messageInteractionId :: InteractionId
  , messageInteractionType :: InteractionType
  , messageInteractionName :: T.Text
  , messageInteractionUser :: User
  } deriving (Show, Eq, Ord, Read)

instance ToJSON MessageInteraction where
  toJSON MessageInteraction{..} = object [(name,value) | (name, Just value) <-
              [ ("id",     toJSON <$> pure messageInteractionId)
              , ("type",   toJSON <$> pure messageInteractionType)
              , ("name",   toJSON <$> pure messageInteractionName)
              , ("user",   toJSON <$> pure messageInteractionUser)
              ] ]


instance FromJSON MessageInteraction where
  parseJSON = withObject "MessageInteraction" $ \o ->
    MessageInteraction <$> o .: "id"
                       <*> o .: "type"
                       <*> o .: "name"
                       <*> o .: "user"

