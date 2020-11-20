{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Data structures pertaining to Discord Channels
module Discord.Internal.Types.Channel where

import Control.Applicative (empty)
import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Text (Text)
import Data.Time.Clock
import qualified Data.Text as T

import Discord.Internal.Types.Prelude
import Discord.Internal.Types.User (User(..))
import Discord.Internal.Types.Embed

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
      , parentId           :: Maybe ParentId    -- ^ The id of the parent channel (category)
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
      , parentId           :: Maybe ParentId
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
      , channelName        :: T.Text
      , channelGuild       :: GuildId
      } deriving (Show, Eq, Ord)

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
                             <*> o .:  "name"
                             <*> o .:? "guild_id" .!= 0
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
      _ -> fail ("Unknown channel type:" <> show type')

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
              , ("parent_id",  toJSON <$> pure parentId)
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
  , overwriteType  :: T.Text    -- ^ Either "role" or "member
  , overwriteAllow :: Integer   -- ^ Allowed permission bit set
  , overwriteDeny  :: Integer   -- ^ Denied permission bit set
  } deriving (Show, Eq, Ord)

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
  { messageId           :: MessageId       -- ^ The id of the message
  , messageChannel      :: ChannelId       -- ^ Id of the channel the message
                                           --   was sent in
  , messageAuthor       :: User            -- ^ The 'User' the message was sent
                                           --   by
  , messageText         :: Text            -- ^ Contents of the message
  , messageTimestamp    :: UTCTime         -- ^ When the message was sent
  , messageEdited       :: Maybe UTCTime   -- ^ When/if the message was edited
  , messageTts          :: Bool            -- ^ Whether this message was a TTS
                                           --   message
  , messageEveryone     :: Bool            -- ^ Whether this message mentions
                                           --   everyone
  , messageMentions     :: [User]          -- ^ 'User's specifically mentioned in
                                           --   the message
  , messageMentionRoles :: [RoleId]        -- ^ 'Role's specifically mentioned in
                                           --   the message
  , messageAttachments  :: [Attachment]    -- ^ Any attached files
  , messageEmbeds       :: [Embed]         -- ^ Any embedded content
  , messageReactions    :: [MessageReaction] -- ^ Any reactions to message
  , messageNonce        :: Maybe Nonce     -- ^ Used for validating if a message
                                           --   was sent
  , messagePinned       :: Bool            -- ^ Whether this message is pinned
  , messageGuild        :: Maybe GuildId   -- ^ The guild the message went to
  } deriving (Show, Eq, Ord)

instance FromJSON Message where
  parseJSON = withObject "Message" $ \o ->
    Message <$> o .:  "id"
            <*> o .:  "channel_id"
            <*> (do isW <- o .:? "webhook_id"
                    a <- o .: "author"
                    case isW :: Maybe WebhookId of
                      Nothing -> pure a
                      Just _ -> pure $ a { userIsWebhook = True })
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
            <*> o .:? "guild_id" .!= Nothing


data MessageReaction = MessageReaction
  { messageReactionCount :: Int
  , messageReactionMeIncluded :: Bool
  , messageReactionEmoji :: Emoji
  } deriving (Show, Eq, Ord)

instance FromJSON MessageReaction where
  parseJSON = withObject "MessageReaction" $ \o ->
    MessageReaction <$> o .: "count"
                    <*> o .: "me"
                    <*> o .: "emoji"

-- | Represents an emoticon (emoji)
data Emoji = Emoji
  { emojiId      :: Maybe EmojiId  -- ^ The emoji id
  , emojiName    :: T.Text         -- ^ The emoji name
  , emojiRoles   :: Maybe [RoleId] -- ^ Roles the emoji is active for
  , emojiUser    :: Maybe User     -- ^ User that created this emoji
  , emojiManaged :: Maybe Bool     -- ^ Whether this emoji is managed
  } deriving (Show, Eq, Ord)

instance FromJSON Emoji where
  parseJSON = withObject "Emoji" $ \o ->
    Emoji <$> o .:  "id"
          <*> o .:  "name"
          <*> o .:? "roles"
          <*> o .:? "user"
          <*> o .:? "managed"

  
-- | Represents an attached to a message file.
data Attachment = Attachment
  { attachmentId       :: Snowflake     -- ^ Attachment id
  , attachmentFilename :: T.Text        -- ^ Name of attached file
  , attachmentSize     :: Integer       -- ^ Size of file (in bytes)
  , attachmentUrl      :: T.Text        -- ^ Source of file
  , attachmentProxy    :: T.Text        -- ^ Proxied url of file
  , attachmentHeight   :: Maybe Integer -- ^ Height of file (if image)
  , attachmentWidth    :: Maybe Integer -- ^ Width of file (if image)
  } deriving (Show, Eq, Ord)

instance FromJSON Attachment where
  parseJSON = withObject "Attachment" $ \o ->
    Attachment <$> o .:  "id"
               <*> o .:  "filename"
               <*> o .:  "size"
               <*> o .:  "url"
               <*> o .:  "proxy_url"
               <*> o .:? "height"
               <*> o .:? "width"



newtype Nonce = Nonce T.Text
  deriving (Show, Eq, Ord)

instance FromJSON Nonce where
  parseJSON (String nonce) = pure $ Nonce nonce
  parseJSON (Number nonce) = pure . Nonce . T.pack . show $ nonce
  parseJSON _ = empty
