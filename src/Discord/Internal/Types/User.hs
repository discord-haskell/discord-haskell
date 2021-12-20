{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Data structures pertaining to Discord User
module Discord.Internal.Types.User where

import Data.Aeson
import Data.Text (Text)
import qualified Data.Text as T
import Discord.Internal.Types.Prelude
import Data.Time (UTCTime)

-- | Represents information about a user.
data User = User
  { userId          :: UserId             -- ^ The user's id.
  , userName        :: T.Text             -- ^ The user's username (not unique)
  , userDiscrim     :: Maybe T.Text       -- ^ The user's 4-digit discord-tag.
  , userAvatar      :: Maybe T.Text       -- ^ The user's avatar hash.
  , userIsBot       :: Bool               -- ^ User is an OAuth2 application.
  , userIsWebhook   :: Bool               -- ^ User is a webhook.
  , userIsSystem    :: Maybe Bool         -- ^ User is an official discord system user.
  , userMfa         :: Maybe Bool         -- ^ User has two factor authentication enabled on the account.
  , userBanner      :: Maybe T.Text       -- ^ User's banner hash
  , userAccentColor :: Maybe Int          -- ^ User's banner color
  , userLocale      :: Maybe T.Text       -- ^ User's chosen language
  , userVerified    :: Maybe Bool         -- ^ Whether the email has been verified.
  , userEmail       :: Maybe T.Text       -- ^ The user's email.
  , userFlags       :: Maybe Integer      -- ^ The user's flags.
  , userPremiumType :: Maybe Integer      -- ^ The user's premium type.
  , userPublicFlags :: Maybe Integer      -- ^ The user's public flags.
  , userMember      :: Maybe GuildMember  -- ^ Some guild member info (message create/update)
  } deriving (Show, Read, Eq, Ord)

instance FromJSON User where
  parseJSON = withObject "User" $ \o ->
    User <$> o .:  "id"
         <*> o .:  "username"
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
  toJSON User{..} = object [(name,value) | (name, Just value) <-
              [ ("id",            toJSON <$> pure userId)
              , ("username",      toJSON <$> pure userName)
              , ("discriminator", toJSON <$>      userDiscrim)
              , ("avatar",        toJSON <$>      userAvatar)
              , ("bot",           toJSON <$> pure userIsBot)
              , ("system",        toJSON <$>      userIsSystem)
              , ("mfa_enabled",   toJSON <$>      userMfa)
              , ("banner",        toJSON <$>      userBanner)
              , ("accent_color",  toJSON <$>      userAccentColor)
              , ("verified",      toJSON <$>      userVerified)
              , ("email",         toJSON <$>      userEmail)
              , ("flags",         toJSON <$>      userFlags)
              , ("premium_type",  toJSON <$>      userPremiumType)
              , ("public_flags",  toJSON <$>      userPublicFlags)
              , ("member",        toJSON <$>      userPublicFlags)
              ] ]

data Webhook = Webhook
  { webhookId :: WebhookId
  , webhookToken :: Text
  , webhookChannelId :: ChannelId
  } deriving (Show, Read, Eq, Ord)

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
  } deriving (Show, Read, Eq, Ord)

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


-- | Representation of a guild member.
data GuildMember = GuildMember
      { memberUser     :: Maybe User -- ^ User object - not included in message_create or update
      , memberNick     :: Maybe T.Text -- ^ User's guild nickname
      , memberAvatar   :: Maybe T.Text -- ^ User's guild avatar hash
      , memberRoles    :: [RoleId] -- ^ Array of role ids
      , memberJoinedAt :: UTCTime -- ^ When the user joined the guild
      , memberPremiumSince :: Maybe UTCTime -- ^ When the user started boosting the guild
      , memberDeaf     :: Bool -- ^ Whether the user is deafened
      , memberMute     :: Bool -- ^ Whether the user is muted
      , memberPending     :: Bool -- ^ Whether the user has passed the guild's membership screening
      , memberPermissions     :: Maybe T.Text -- ^ total permissions of the member
      } deriving (Show, Read, Eq, Ord)

instance FromJSON GuildMember where
  parseJSON = withObject "GuildMember" $ \o ->
    GuildMember <$> o .:? "user"
                <*> o .:? "nick"
                <*> o .:? "avatar"
                <*> o .:  "roles"
                <*> o .:  "joined_at"
                <*> o .:? "premium_since"
                <*> o .:  "deaf"
                <*> o .:  "mute"
                <*> o .:? "pending" .!= False
                <*> o .:? "permissions"

instance ToJSON GuildMember where
  toJSON GuildMember {..} = object [(name, value) | (name, Just value) <-
      [ ("user",      toJSON <$>      memberUser)
      , ("nick",      toJSON <$>      memberNick)
      , ("avatar",    toJSON <$>      memberAvatar)
      , ("roles",     toJSON <$> pure memberRoles)
      , ("joined_at", toJSON <$> pure memberJoinedAt)
      , ("premium_since",    toJSON <$>      memberPremiumSince)
      , ("deaf",      toJSON <$> pure memberDeaf)
      , ("mute",      toJSON <$> pure memberMute)
      , ("pending",      toJSON <$> pure memberPending)
      , ("permissions",    toJSON <$>      memberPermissions)
      ] ]
