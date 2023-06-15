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
  , userGlobalName  :: Maybe T.Text       -- ^ The user's display name.
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
         <*> o .:? "global_name"
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
  toJSON User{..} = objectFromMaybes
              [ "id" .== userId
              , "username" .== userName
              , "discriminator" .=? userDiscrim
              , "global_name" .=? userGlobalName
              , "avatar" .=? userAvatar
              , "bot" .== userIsBot
              , "system" .=? userIsSystem
              , "mfa_enabled" .=? userMfa
              , "banner" .=? userBanner
              , "accent_color" .=? userAccentColor
              , "verified" .=? userVerified
              , "email" .=? userEmail
              , "flags" .=? userFlags
              , "premium_type" .=? userPremiumType
              , "public_flags" .=? userPublicFlags
              , "member" .=? userPublicFlags
              ]

-- TODO: fully update webhook structure
data Webhook = Webhook
  { webhookId :: WebhookId
  , webhookToken :: Maybe WebhookToken
  , webhookChannelId :: ChannelId
  } deriving (Show, Read, Eq, Ord)

instance FromJSON Webhook where
  parseJSON = withObject "Webhook" $ \o ->
    Webhook <$> o .:  "id"
            <*> o .:? "token"
            <*> o .:  "channel_id"

-- | The connection object that the user has attached.
data ConnectionObject = ConnectionObject
  { connectionObjectId :: Text -- ^ id of the connection account
  , connectionObjectName :: Text -- ^ the username of the connection account
  , connectionObjectType :: Text -- ^ the service of the connection (twitch, youtube)
  , connectionObjectRevoked :: Bool -- ^ whether the connection is revoked
  , connectionObjectIntegrations :: [IntegrationId] -- ^ List of server `IntegrationId`
  , connectionObjectVerified :: Bool -- ^ whether the connection is verified
  , connectionObjectFriendSyncOn :: Bool -- ^ whether friend sync is enabled for this connection
  , connectionObjectShownInPresenceUpdates :: Bool -- ^ whether activities related to this connection will be shown in presence updates
  , connectionObjectVisibleToOthers :: Bool -- ^ visibility of this connection
  } deriving (Show, Read, Eq, Ord)

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
      , memberTimeoutEnd :: Maybe UTCTime -- ^ when the user's timeout will expire and they can communicate again
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
                <*> o .:? "communication_disabled_until"

instance ToJSON GuildMember where
  toJSON GuildMember {..} = objectFromMaybes
      [ "user" .=? memberUser
      , "nick" .=? memberNick
      , "avatar" .=? memberAvatar
      , "roles" .== memberRoles
      , "joined_at" .== memberJoinedAt
      , "premium_since" .=? memberPremiumSince
      , "deaf" .== memberDeaf
      , "mute" .== memberMute
      , "pending" .== memberPending
      , "permissions" .=? memberPermissions
      , "communication_disabled_until" .=? memberTimeoutEnd
      ]
