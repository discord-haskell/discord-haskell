{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Data structures pertaining to Discord Channels
module Discord.Internal.Types.Channel where

import Control.Applicative (empty)
import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Default (Default, def)
import Data.Monoid ((<>))
import Data.Text (Text)
import Data.Time.Clock
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V
import qualified Data.Text as T

import Discord.Internal.Types.Prelude

-- | Represents information about a user.
data User = User
  { userId       :: UserId       -- ^ The user's id.
  , userName     :: T.Text       -- ^ The user's username (not unique)
  , userDiscrim  :: T.Text       -- ^ The user's 4-digit discord-tag.
  , userAvatar   :: Maybe T.Text -- ^ The user's avatar hash.
  , userIsBot    :: Bool         -- ^ User is an OAuth2 application.
  , userIsWebhook:: Bool         -- ^ User is a webhook
  , userMfa      :: Maybe Bool   -- ^ User has two factor authentication enabled on the account.
  , userVerified :: Maybe Bool   -- ^ Whether the email has been verified.
  , userEmail    :: Maybe T.Text -- ^ The user's email.
  } deriving (Show, Eq, Ord)

instance FromJSON User where
  parseJSON = withObject "User" $ \o ->
    User <$> o .:  "id"
         <*> o .:  "username"
         <*> o .:  "discriminator"
         <*> o .:? "avatar"
         <*> o .:? "bot" .!= False
         <*> pure False -- webhook
         <*> o .:? "mfa_enabled"
         <*> o .:? "verified"
         <*> o .:? "email"

instance ToJSON User where
  toJSON User{..} = object [(name,value) | (name, Just value) <-
              [ ("id",            toJSON <$> pure userId)
              , ("username",      toJSON <$> pure userName)
              , ("discriminator", toJSON <$> pure userDiscrim)
              , ("avatar",        toJSON <$>      userAvatar)
              , ("bot",           toJSON <$> pure userIsBot)
              , ("webhook",       toJSON <$> pure userIsWebhook)
              , ("mfa_enabled",   toJSON <$>      userMfa)
              , ("verified",      toJSON <$>      userVerified)
              , ("email",         toJSON <$>      userEmail)
              ] ]

data Webhook = Webhook
  { webhookId :: WebhookId
  , webhookToken :: Text
  , webhookChannelId :: ChannelId
  } deriving (Show, Eq, Ord)

instance FromJSON Webhook where
  parseJSON = withObject "Webhook" $ \o ->
    Webhook <$> o .:  "id"
            <*> o .:  "token"
            <*> o .:  "channel_id"

data ConnectionObject = ConnectionObject
  { connectionObjectId :: Text
  , connectionObjectName :: Text
  , connectionObjectType :: Text
  , connectionObjectRevoked :: Bool
  , connectionObjectIntegrations :: [IntegrationId]
  , connectionObjectVerified :: Bool
  , connectionObjectFriendSyncOn :: Bool
  , connectionObjectShownInPresenceUpdates :: Bool
  , connectionObjectVisibleToOthers :: Bool
  } deriving (Show, Eq, Ord)

instance FromJSON ConnectionObject where
  parseJSON = withObject "ConnectionObject" $ \o -> do
    integrations <- o .: "integrations"
    ConnectionObject <$> o .: "id"
               <*> o .: "name"
               <*> o .: "type"
               <*> o .: "revoked"
               <*> sequence (map (.: "id") integrations)
               <*> o .: "verified"
               <*> o .: "friend_sync"
               <*> o .: "show_activity"
               <*> ( (==) (1::Int) <$> o .: "visibility")


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
      , channelTopic       :: T.Text      -- ^ The topic of the channel. (0 - 1024 chars).
      , channelLastMessage :: Maybe MessageId   -- ^ The id of the last message sent in the
                                                --   channel
      }
  -- | A voice channel in a guild.
  | ChannelVoice
      { channelId          :: ChannelId
      , channelGuild       :: GuildId
      , channelName        :: T.Text
      , channelPosition    :: Integer
      , channelPermissions :: [Overwrite]
      , channelBitRate     :: Integer     -- ^ The bitrate (in bits) of the channel.
      , channelUserLimit   :: Integer     -- ^ The user limit of the voice channel.
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
                     <*> o .:? "topic" .!= ""
                     <*> o .:? "last_message_id"
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
                     <*> o .:  "bitrate"
                     <*> o .:  "user_limit"
      3 ->
        ChannelGroupDM <$> o .:  "id"
                       <*> o .:  "recipients"
                       <*> o .:? "last_message_id"
      4 ->
        ChannelGuildCategory <$> o .: "id"
                             <*> o .:? "guild_id" .!= 0
      _ -> fail ("Unknown channel type:" <> show type')

instance ToJSON Channel where
  toJSON ChannelText{..} = object [(name,value) | (name, Just value) <-
              [ ("id",     toJSON <$> pure channelId)
              , ("guild_id", toJSON <$> pure channelGuild)
              , ("name",  toJSON <$> pure channelName)
              , ("position",   toJSON <$> pure channelPosition)
              , ("permission_overwrites",   toJSON <$> pure channelPermissions)
              , ("topic",   toJSON <$> pure channelTopic)
              , ("last_message_id",  toJSON <$> channelLastMessage)
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
              , ("guild_id", toJSON <$> pure channelGuild)
              ] ]

-- | If the channel is part of a guild (has a guild id field)
channelIsInGuild :: Channel -> Bool
channelIsInGuild c = case c of
        ChannelGuildCategory{..} -> True
        ChannelText{..} -> True
        ChannelVoice{..}  -> True
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
  , messageEmbeds       :: [OldEmbed]      -- ^ Any embedded content
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
            <*> o .:? "nonce"
            <*> o .:? "pinned" .!= False
            <*> o .:? "guild_id" .!= Nothing

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


-- | An embed attached to a message.
data Embed = Embed
  { embedAuthor      :: Maybe EmbedAuthor
  , embedTitle       :: Maybe T.Text     -- ^ Title of the embed
  , embedUrl         :: Maybe T.Text     -- ^ URL of embed
  , embedDescription :: Maybe T.Text     -- ^ Description of embed
  , embedFields      :: [EmbedField]     -- ^ Fields of the embed
  , embedImage       :: Maybe EmbedImage
  , embedFooter      :: Maybe EmbedFooter

  , embedColor       :: Maybe Integer    -- ^ The embed color
  , embedTimestamp   :: Maybe UTCTime    -- ^ The time of the embed content
  , embedType        :: Maybe T.Text     -- ^ Type of embed (Always "rich" for users)
  , embedVideo       :: Maybe EmbedVideo -- ^ Only present for "video" types
  , embedProvider    :: Maybe EmbedProvider -- ^ Only present for "video" types
  } deriving (Show, Eq, Ord)

instance FromJSON Embed where
  parseJSON = withObject "embed" $ \o ->
    Embed <$> o .:? "author"
          <*> o .:? "title"
          <*> o .:? "url"
          <*> o .:? "description"
          <*> o .:  "fields"
          <*> o .:? "image"
          <*> o .:? "footer"
          <*> o .:? "color"
          <*> o .:? "timestamp"
          <*> o .:? "type"
          <*> o .:? "video"
          <*> o .:? "provider"


-- | An embed attached to a message.
data OldEmbed = OldEmbed
  { oldembedTitle       :: Maybe T.Text     -- ^ Title of the embed
  , oldembedDescription :: Maybe T.Text     -- ^ Description of embed
  , oldembedUrl         :: Maybe T.Text     -- ^ URL of embed
  , oldembedTimestamp   :: Maybe UTCTime    -- ^ The time of the embed content
  , oldembedColor       :: Maybe Integer    -- ^ The embed color
  , oldembedFields      :: [EmbedPart]      -- ^ Fields of the embed
  , oldembedType        :: Maybe T.Text     -- ^ Type of embed (Always "rich" for webhooks)
  } deriving (Show, Eq, Ord)

instance FromJSON OldEmbed where
  parseJSON = withObject "Embed" $ \o ->
    OldEmbed <$> o .:? "title"
          <*> o .:? "description"
          <*> o .:? "url"
          <*> o .:? "timestamp"
          <*> o .:? "color"
          <*> sequence (HM.foldrWithKey to_embed [] o)
          <*> o .:? "type"
    where
      to_embed k (Object v) a = case k of
        "footer" -> (Footer <$> v .: "text"
                            <*> v .:? "icon_url" .!= ""
                            <*> v .:? "proxy_icon_url" .!= "") : a
        "image" -> (Image <$> v .: "url"
                          <*> v .: "proxy_url"
                          <*> v .: "height"
                          <*> v .: "width") : a
        "thumbnail" -> (Thumbnail <$> v .: "url"
                                  <*> v .: "proxy_url"
                                  <*> v .: "height"
                                  <*> v .: "width") : a
        "video" -> (Video <$> v .: "url"
                          <*> v .: "height"
                          <*> v .: "width") : a
        "provider" -> (Provider <$> v .: "name"
                                <*> v .:? "url" .!= "") : a
        "author" -> (Author <$> v .:  "name"
                            <*> v .:?  "url" .!= ""
                            <*> v .:? "icon_url" .!= ""
                            <*> v .:? "proxy_icon_url" .!= "") : a
        _ -> a
      to_embed k (Array v) a = case k of
        "fields" -> [Field <$> i .: "name"
                           <*> i .: "value"
                           <*> i .: "inline"
                           | Object i <- V.toList v] ++ a
        _ -> a
      to_embed _ _ a = a

instance ToJSON OldEmbed where
  toJSON (OldEmbed {..}) = object
    [ "title"       .= oldembedTitle
    , "type"        .= oldembedType
    , "description" .= oldembedDescription
    , "url"         .= oldembedUrl
    , "timestamp"   .= oldembedTimestamp
    , "color"       .= oldembedColor
    ] |> makeSubEmbeds oldembedFields
    where
      (|>) :: Value -> HM.HashMap Text Value -> Value
      (|>) (Object o) hm = Object $ HM.union o hm
      (|>) _          _  = error "Type mismatch"

      makeSubEmbeds :: [EmbedPart] -> HM.HashMap Text Value
      makeSubEmbeds = foldr embed HM.empty

      embed :: EmbedPart -> HM.HashMap Text Value -> HM.HashMap Text Value
      embed (Thumbnail url _ height width) =
        HM.alter (\_ -> Just $ object
          [ "url"    .= url
          , "height" .= height
          , "width"  .= width
          ]) "thumbnail"
      embed (Image url _ height width) =
        HM.alter (\_ -> Just $ object
          [ "url"    .= url
          , "height" .= height
          , "width"  .= width
          ]) "image"
      embed (Author name url icon _) =
        HM.alter (\_ -> Just $ object
          [ "name"     .= name
          , "url"      .= url
          , "icon_url" .= icon
          ]) "author"
      embed (Footer text icon _) =
        HM.alter (\_ -> Just $ object
          [ "text"     .= text
          , "icon_url" .= icon
          ]) "footer"
      embed (Field name value inline) =
        HM.alter (\val -> case val of
          Just (Array a) -> Just . Array $ V.cons (object
            [ "name"   .= name
            , "value"  .= value
            , "inline" .= inline
            ]) a
          _ -> Just $ toJSON [
            object
              [ "name"   .= name
              , "value"  .= value
              , "inline" .= inline
              ]
            ]
        ) "fields"
      embed _ = id

-- | Represents a part of an embed.
data EmbedPart
  = Thumbnail
      T.Text
      T.Text
      Integer
      Integer
  | Video
      T.Text
      Integer
      Integer
  | Image
      T.Text
      T.Text
      Integer
      Integer
  | Provider
      T.Text
      T.Text
  | Author
      T.Text
      T.Text
      T.Text
      T.Text
  | Footer
      T.Text
      T.Text
      T.Text
  | Field
      T.Text
      T.Text
      Bool
  deriving (Show, Eq, Ord)


data EmbedThumbnail = EmbedThumbnail
  { embedThumbnailUrl :: Maybe T.Text
  , embedThumbnailProxyUrl :: Maybe T.Text
  , embedThumbnailHeight :: Maybe Integer
  , embedThumbnailWidth :: Maybe Integer
  } deriving (Show, Eq, Ord)

instance FromJSON EmbedThumbnail where
  parseJSON = withObject "thumbnail" $ \o ->
    EmbedThumbnail <$> o .:? "url"
                   <*> o .:? "proxy_url"
                   <*> o .:? "height"
                   <*> o .:? "width"

data EmbedVideo = EmbedVideo
  { embedVideoUrl :: Maybe T.Text
  , embedVideoHeight :: Maybe Integer
  , embedVideoWidth :: Maybe Integer
  } deriving (Show, Eq, Ord)

instance FromJSON EmbedVideo where
  parseJSON = withObject "video" $ \o ->
    EmbedVideo <$> o .:? "url"
               <*> o .:? "height"
               <*> o .:? "width"

data EmbedImage = EmbedImage
  { embedImageUrl :: Maybe T.Text
  , embedImageProxyUrl :: Maybe T.Text
  , embedImageHeight :: Maybe Integer
  , embedImageWidth :: Maybe Integer
  } deriving (Show, Eq, Ord)

instance FromJSON EmbedImage where
  parseJSON = withObject "image" $ \o ->
    EmbedImage <$> o .:? "url"
               <*> o .:? "proxy_url"
               <*> o .:? "height"
               <*> o .:? "width"

data EmbedProvider = EmbedProvider
  { embedProviderName :: Maybe T.Text
  , embedProviderUrl :: Maybe T.Text
  } deriving (Show, Eq, Ord)

instance FromJSON EmbedProvider where
  parseJSON = withObject "provider" $ \o ->
    EmbedProvider <$> o .:? "name"
                  <*> o .:? "url"

data EmbedAuthor = EmbedAuthor
  { embedAuthorName :: Maybe T.Text
  , embedAuthorUrl :: Maybe T.Text
  , embedAuthorIconUrl :: Maybe T.Text
  , embedAuthorProxyIconUrl :: Maybe T.Text
  } deriving (Show, Eq, Ord)

instance FromJSON EmbedAuthor where
  parseJSON = withObject "author" $ \o ->
    EmbedAuthor <$> o .:? "name"
                <*> o .:? "url"
                <*> o .:? "icon_url"
                <*> o .:? "proxy_icon_url"

data EmbedFooter = EmbedFooter
  { embedFooterText :: T.Text
  , embedFooterIconUrl :: Maybe T.Text
  , embedFooterProxyIconUrl :: Maybe T.Text
  } deriving (Show, Eq, Ord)

instance FromJSON EmbedFooter where
  parseJSON = withObject "footer" $ \o ->
    EmbedFooter <$> o .:  "text"
                <*> o .:  "icon_url"
                <*> o .:? "proxy_icon_url"

data EmbedField = EmbedField
  { embedFieldName :: T.Text
  , embedFieldValue :: T.Text
  , embedFieldInline :: Maybe Bool
  } deriving (Show, Eq, Ord)

instance FromJSON EmbedField where
  parseJSON = withObject "field" $ \o ->
    EmbedField <$> o .:  "name"
               <*> o .:  "value"
               <*> o .:? "inline"


newtype Nonce = Nonce T.Text
  deriving (Show, Eq, Ord)

instance FromJSON Nonce where
  parseJSON (String nonce) = pure $ Nonce nonce
  parseJSON (Number nonce) = pure . Nonce . T.pack . show $ nonce
  parseJSON _ = empty
