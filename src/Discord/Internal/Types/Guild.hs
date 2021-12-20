{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Types relating to Discord Guilds (servers)
module Discord.Internal.Types.Guild where

import Data.Time.Clock

import Data.Aeson
import qualified Data.Text as T

import Discord.Internal.Types.Prelude
import Discord.Internal.Types.Channel (Channel)
import Discord.Internal.Types.User (User, GuildMember)
import Discord.Internal.Types.Components (Emoji)



-- | Guilds in Discord represent a collection of users and channels into an isolated
--   "Server"
--
-- https://discord.com/developers/docs/resources/guild#guild-object
data Guild = Guild
      { guildId                  :: GuildId       -- ^ Gulid id
      , guildName                :: T.Text          -- ^ Guild name (2 - 100 chars)
      , guildIcon                :: Maybe T.Text    -- ^ Icon hash
      , guildSplash              :: Maybe T.Text    -- ^ Splash hash
      , guildOwnerId             :: UserId       -- ^ Guild owner id
      , guildPermissions         :: Maybe T.Text
      , guildRegion              :: Maybe T.Text    -- ^ Guild voice region
      , guildAfkId               :: Maybe ChannelId -- ^ Id of afk channel
      , guildAfkTimeout          :: Integer         -- ^ Afk timeout in seconds
      , guildWidgetEnabled       :: Maybe Bool      -- ^ Id of embedded channel
      , guildWidgetChannelId     :: Maybe ChannelId -- ^ Id of embedded channel
      , guildVerificationLevel   :: Integer         -- ^ Level of verification
      , guildNotification        :: Integer         -- ^ Level of default notifications
      , guildExplicitFilterLevel :: Integer
      , guildRoles               :: [Role]           -- ^ Array of 'Role' objects
      , guildEmojis              :: [Emoji]          -- ^ Array of 'Emoji' objects
      , guildFeatures            :: [T.Text]
      , guildMultiFactAuth       :: !Integer
      , guildApplicationId       :: Maybe Snowflake
      } deriving (Show, Read, Eq, Ord)

instance FromJSON Guild where
  parseJSON = withObject "Guild" $ \o ->
    Guild <$> o .:  "id"
          <*> o .:  "name"
          <*> o .:? "icon"
          <*> o .:? "splash"
          <*> o .:  "owner_id"
          <*> o .:? "permissions"
          <*> o .:? "region"
          <*> o .:? "afk_channel_id"
          <*> o .:  "afk_timeout"
          <*> o .:? "widget_enabled"
          <*> o .:? "widget_channel_id"
          <*> o .:  "verification_level"
          <*> o .:  "default_message_notifications"
          <*> o .:  "explicit_content_filter"
          <*> o .:  "roles"
          <*> o .:  "emojis"
          <*> o .:  "features"
          <*> o .:  "mfa_level"
          <*> o .:? "application_id"

data GuildUnavailable = GuildUnavailable
      { idOnceAvailable :: GuildId
      } deriving (Show, Read, Eq, Ord)

instance FromJSON GuildUnavailable where
  parseJSON = withObject "GuildUnavailable" $ \o ->
       GuildUnavailable <$> o .: "id"

data GuildInfo = GuildInfo
      { guildJoinedAt    :: UTCTime
      , guildLarge       :: Bool
      , guildMemberCount :: Integer
   -- , guildVoiceStates :: [VoiceState]  -- (without guildid) todo have to add voice state data type
      , guildMembers     :: [GuildMember]
      , guildChannels    :: [Channel]     -- ^ Channels in the guild (sent in GuildCreate)
   -- , guildPresences   :: [Presence]
      } deriving (Show, Read, Eq, Ord)

instance FromJSON GuildInfo where
  parseJSON = withObject "GuildInfo" $ \o ->
    GuildInfo <$> o .: "joined_at"
              <*> o .: "large"
              <*> o .: "member_count"
           -- <*> o .: "voice_states"
              <*> o .: "members"
              <*> o .: "channels"

data PartialGuild = PartialGuild
      { partialGuildId          :: GuildId
      , partialGuildName        :: T.Text
      , partialGuildIcon        :: Maybe T.Text
      , partialGuildOwner       :: Bool
      , partialGuildPermissions :: T.Text
      } deriving (Show, Read, Eq, Ord)

instance FromJSON PartialGuild where
  parseJSON = withObject "PartialGuild" $ \o ->
    PartialGuild <$> o .:  "id"
                 <*> o .:  "name"
                 <*> o .:? "icon"
                 <*> o .:?  "owner" .!= False
                 <*> o .:  "permissions"


-- | Roles represent a set of permissions attached to a group of users. Roles have unique
--   names, colors, and can be "pinned" to the side bar, causing their members to be listed separately.
--   Roles are unique per guild, and can have separate permission profiles for the global context
--   (guild) and channel context.
data Role =
    Role {
        roleId      :: RoleId -- ^ The role id
      , roleName    :: T.Text                    -- ^ The role name
      , roleColor   :: ColorInteger              -- ^ Integer representation of color code
      , roleHoist   :: Bool                      -- ^ If the role is pinned in the user listing
      , rolePos     :: Integer                   -- ^ Position of this role
      , rolePerms   :: T.Text                   -- ^ Permission bit set
      , roleManaged :: Bool                      -- ^ Whether this role is managed by an integration
      , roleMention :: Bool                      -- ^ Whether this role is mentionable
    } deriving (Show, Read, Eq, Ord)

instance FromJSON Role where
  parseJSON = withObject "Role" $ \o ->
    Role <$> o .: "id"
         <*> o .: "name"
         <*> o .: "color"
         <*> o .: "hoist"
         <*> o .: "position"
         <*> o .: "permissions"
         <*> o .: "managed"
         <*> o .: "mentionable"

-- | VoiceRegion is only refrenced in Guild endpoints, will be moved when voice support is added
data VoiceRegion = VoiceRegion
      { voiceRegionId          :: T.Text      -- ^ Unique id of the region
      , voiceRegionName        :: T.Text      -- ^ Name of the region
      , voiceRegionVip         :: Bool        -- ^ True if this is a VIP only server
      , voiceRegionOptimal     :: Bool        -- ^ True for the closest server to a client
      , voiceRegionDeprecated  :: Bool        -- ^ Whether this is a deprecated region
      , voiceRegionCustom      :: Bool        -- ^ Whether this is a custom region
      } deriving (Show, Read, Eq, Ord)

instance FromJSON VoiceRegion where
  parseJSON = withObject "VoiceRegion" $ \o ->
    VoiceRegion <$> o .: "id"
                <*> o .: "name"
                <*> o .: "vip"
                <*> o .: "optimal"
                <*> o .: "deprecated"
                <*> o .: "custom"

-- | Info about a Ban
data GuildBan = GuildBan
      { guildBanReason  :: T.Text
      , guildBanUser    :: User
      } deriving (Show, Read, Eq, Ord)

instance FromJSON GuildBan where
  parseJSON = withObject "GuildBan" $ \o -> GuildBan <$> o .: "reason" <*> o .: "user"

-- | Represents a code to add a user to a guild
data Invite = Invite
      { inviteCode  :: T.Text    -- ^ The invite code
      , inviteGuildId :: Maybe GuildId -- ^ The guild the code will invite to
      , inviteChannelId :: ChannelId -- ^ The channel the code will invite to
      } deriving (Show, Read, Eq, Ord)

instance FromJSON Invite where
  parseJSON = withObject "Invite" $ \o ->
    Invite <$>  o .: "code"
           <*> (do g <- o .:? "guild"
                   case g of Just g2 -> g2 .: "id"
                             Nothing -> pure Nothing)
           <*> ((o .:  "channel") >>= (.: "id"))

-- | Invite code with additional metadata
data InviteWithMeta = InviteWithMeta Invite InviteMeta

instance FromJSON InviteWithMeta where
  parseJSON ob = InviteWithMeta <$> parseJSON ob <*> parseJSON ob

-- | Additional metadata about an invite.
data InviteMeta = InviteMeta
    { inviteCreator :: User    -- ^ The user that created the invite
    , inviteUses    :: Integer -- ^ Number of times the invite has been used
    , inviteMax     :: Integer -- ^ Max number of times the invite can be used
    , inviteAge     :: Integer -- ^ The duration (in seconds) after which the invite expires
    , inviteTemp    :: Bool    -- ^ Whether this invite only grants temporary membership
    , inviteCreated :: UTCTime -- ^ When the invite was created
    , inviteRevoked :: Bool    -- ^ If the invite is revoked
    } deriving (Show, Read, Eq, Ord)

instance FromJSON InviteMeta where
  parseJSON = withObject "InviteMeta" $ \o ->
    InviteMeta <$> o .: "inviter"
               <*> o .: "uses"
               <*> o .: "max_uses"
               <*> o .: "max_age"
               <*> o .: "temporary"
               <*> o .: "created_at"
               <*> o .: "revoked"

-- | Represents the behavior of a third party account link.
data Integration = Integration
      { integrationId       :: !Snowflake -- ^ Integration id
      , integrationName     :: T.Text                    -- ^ Integration name
      , integrationType     :: T.Text                    -- ^ Integration type (Twitch, Youtube, ect.)
      , integrationEnabled  :: Bool                      -- ^ Is the integration enabled
      , integrationSyncing  :: Bool                      -- ^ Is the integration syncing
      , integrationRole     :: RoleId                 -- ^ Id the integration uses for "subscribers"
      , integrationBehavior :: Integer                   -- ^ The behavior of expiring subscribers
      , integrationGrace    :: Integer                   -- ^ The grace period before expiring subscribers
      , integrationOwner    :: User                      -- ^ The user of the integration
      , integrationAccount  :: IntegrationAccount        -- ^ The account the integration links to
      , integrationSync     :: UTCTime                   -- ^ When the integration was last synced
      } deriving (Show, Read, Eq, Ord)

instance FromJSON Integration where
  parseJSON = withObject "Integration" $ \o ->
    Integration <$> o .: "id"
                <*> o .: "name"
                <*> o .: "type"
                <*> o .: "enabled"
                <*> o .: "syncing"
                <*> o .: "role_id"
                <*> o .: "expire_behavior"
                <*> o .: "expire_grace_period"
                <*> o .: "user"
                <*> o .: "account"
                <*> o .: "synced_at"

-- | Represents a third party account link.
data IntegrationAccount = IntegrationAccount
    { accountId   :: T.Text -- ^ The id of the account.
    , accountName :: T.Text -- ^ The name of the account.
    } deriving (Show, Read, Eq, Ord)

instance FromJSON IntegrationAccount where
  parseJSON = withObject "IntegrationAccount" $ \o ->
    IntegrationAccount <$> o .: "id" <*> o .: "name"

-- | Represents an image to be used in third party sites to link to a discord channel
data GuildWidget = GuildWidget
      { widgetEnabled :: Bool      -- ^ Whether the widget is enabled
      , widgetChannelId :: ChannelId -- ^ The widget channel id
      } deriving (Show, Read, Eq, Ord)

instance FromJSON GuildWidget where
  parseJSON = withObject "GuildWidget" $ \o ->
    GuildWidget <$> o .: "enabled" <*> o .: "channel_id"

instance ToJSON GuildWidget where
  toJSON (GuildWidget enabled snowflake) = object
    [ "enabled"   .= enabled
    , "channel_id" .= snowflake
    ]
