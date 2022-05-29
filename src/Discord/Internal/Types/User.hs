{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Data structures pertaining to Discord User
module Discord.Internal.Types.User where

import Data.Aeson
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime)
import Discord.Internal.Types.Prelude

-- | Represents information about a user.
data User = User
  { -- | The user's id.
    userId :: UserId,
    -- | The user's username (not unique)
    userName :: T.Text,
    -- | The user's 4-digit discord-tag.
    userDiscrim :: Maybe T.Text,
    -- | The user's avatar hash.
    userAvatar :: Maybe T.Text,
    -- | User is an OAuth2 application.
    userIsBot :: Bool,
    -- | User is a webhook.
    userIsWebhook :: Bool,
    -- | User is an official discord system user.
    userIsSystem :: Maybe Bool,
    -- | User has two factor authentication enabled on the account.
    userMfa :: Maybe Bool,
    -- | User's banner hash
    userBanner :: Maybe T.Text,
    -- | User's banner color
    userAccentColor :: Maybe Int,
    -- | User's chosen language
    userLocale :: Maybe T.Text,
    -- | Whether the email has been verified.
    userVerified :: Maybe Bool,
    -- | The user's email.
    userEmail :: Maybe T.Text,
    -- | The user's flags.
    userFlags :: Maybe Integer,
    -- | The user's premium type.
    userPremiumType :: Maybe Integer,
    -- | The user's public flags.
    userPublicFlags :: Maybe Integer,
    -- | Some guild member info (message create/update)
    userMember :: Maybe GuildMember
  }
  deriving (Show, Read, Eq, Ord)

instance FromJSON User where
  parseJSON = withObject "User" $ \o ->
    User <$> o .: "id"
      <*> o .: "username"
      <*> o .:? "discriminator" -- possibly not there in the case of webhooks
      <*> o .:? "avatar"
      <*> o .:? "bot" .!= False
      <*> pure False -- webhook
      <*> o .:? "system"
      <*> o .:? "mfa_enabled"
      <*> o .:? "banner"
      <*> o .:? "accent_color"
      <*> o .:? "locale"
      <*> o .:? "verified"
      <*> o .:? "email"
      <*> o .:? "flags"
      <*> o .:? "premium_type"
      <*> o .:? "public_flags"
      <*> o .:? "member"

instance ToJSON User where
  toJSON User {..} =
    object
      [ (name, value)
        | (name, Just value) <-
            [ ("id", toJSON <$> pure userId),
              ("username", toJSON <$> pure userName),
              ("discriminator", toJSON <$> userDiscrim),
              ("avatar", toJSON <$> userAvatar),
              ("bot", toJSON <$> pure userIsBot),
              ("system", toJSON <$> userIsSystem),
              ("mfa_enabled", toJSON <$> userMfa),
              ("banner", toJSON <$> userBanner),
              ("accent_color", toJSON <$> userAccentColor),
              ("verified", toJSON <$> userVerified),
              ("email", toJSON <$> userEmail),
              ("flags", toJSON <$> userFlags),
              ("premium_type", toJSON <$> userPremiumType),
              ("public_flags", toJSON <$> userPublicFlags),
              ("member", toJSON <$> userPublicFlags)
            ]
      ]

-- TODO: fully update webhook structure
data Webhook = Webhook
  { webhookId :: WebhookId,
    webhookToken :: Maybe Text,
    webhookChannelId :: ChannelId
  }
  deriving (Show, Read, Eq, Ord)

instance FromJSON Webhook where
  parseJSON = withObject "Webhook" $ \o ->
    Webhook <$> o .: "id"
      <*> o .:? "token"
      <*> o .: "channel_id"

-- | The connection object that the user has attached.
data ConnectionObject = ConnectionObject
  { -- | id of the connection account
    connectionObjectId :: Text,
    -- | the username of the connection account
    connectionObjectName :: Text,
    -- | the service of the connection (twitch, youtube)
    connectionObjectType :: Text,
    -- | whether the connection is revoked
    connectionObjectRevoked :: Bool,
    -- | List of server `IntegrationId`
    connectionObjectIntegrations :: [IntegrationId],
    -- | whether the connection is verified
    connectionObjectVerified :: Bool,
    -- | whether friend sync is enabled for this connection
    connectionObjectFriendSyncOn :: Bool,
    -- | whether activities related to this connection will be shown in presence updates
    connectionObjectShownInPresenceUpdates :: Bool,
    -- | visibility of this connection
    connectionObjectVisibleToOthers :: Bool
  }
  deriving (Show, Read, Eq, Ord)

instance FromJSON ConnectionObject where
  parseJSON = withObject "ConnectionObject" $ \o -> do
    integrations <- o .: "integrations"
    ConnectionObject <$> o .: "id"
      <*> o .: "name"
      <*> o .: "type"
      <*> o .: "revoked"
      <*> mapM (.: "id") integrations
      <*> o .: "verified"
      <*> o .: "friend_sync"
      <*> o .: "show_activity"
      <*> ((==) (1 :: Int) <$> o .: "visibility")

-- | Representation of a guild member.
data GuildMember = GuildMember
  { -- | User object - not included in message_create or update
    memberUser :: Maybe User,
    -- | User's guild nickname
    memberNick :: Maybe T.Text,
    -- | User's guild avatar hash
    memberAvatar :: Maybe T.Text,
    -- | Array of role ids
    memberRoles :: [RoleId],
    -- | When the user joined the guild
    memberJoinedAt :: UTCTime,
    -- | When the user started boosting the guild
    memberPremiumSince :: Maybe UTCTime,
    -- | Whether the user is deafened
    memberDeaf :: Bool,
    -- | Whether the user is muted
    memberMute :: Bool,
    -- | Whether the user has passed the guild's membership screening
    memberPending :: Bool,
    -- | total permissions of the member
    memberPermissions :: Maybe T.Text,
    -- | when the user's timeout will expire and they can communicate again
    memberTimeoutEnd :: Maybe UTCTime
  }
  deriving (Show, Read, Eq, Ord)

instance FromJSON GuildMember where
  parseJSON = withObject "GuildMember" $ \o ->
    GuildMember <$> o .:? "user"
      <*> o .:? "nick"
      <*> o .:? "avatar"
      <*> o .: "roles"
      <*> o .: "joined_at"
      <*> o .:? "premium_since"
      <*> o .: "deaf"
      <*> o .: "mute"
      <*> o .:? "pending" .!= False
      <*> o .:? "permissions"
      <*> o .:? "communication_disabled_until"

instance ToJSON GuildMember where
  toJSON GuildMember {..} =
    object
      [ (name, value)
        | (name, Just value) <-
            [ ("user", toJSON <$> memberUser),
              ("nick", toJSON <$> memberNick),
              ("avatar", toJSON <$> memberAvatar),
              ("roles", toJSON <$> pure memberRoles),
              ("joined_at", toJSON <$> pure memberJoinedAt),
              ("premium_since", toJSON <$> memberPremiumSince),
              ("deaf", toJSON <$> pure memberDeaf),
              ("mute", toJSON <$> pure memberMute),
              ("pending", toJSON <$> pure memberPending),
              ("permissions", toJSON <$> memberPermissions),
              ("communication_disabled_until", toJSON <$> memberTimeoutEnd)
            ]
      ]
