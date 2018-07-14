{-# LANGUAGE OverloadedStrings #-}

-- | Types relating to Discord Guilds (servers)
module Discord.Types.Guild where

import Data.Time.Clock

import Data.Aeson

import Discord.Types.Channel
import Discord.Types.Prelude

-- |Representation of a guild member.
data GuildMember = GuildMember
      { memberUser     :: User
      , memberNick     :: Maybe String
      , memberRoles    :: [Snowflake]
      , memberJoinedAt :: UTCTime
      , memberDeaf     :: Bool
      , memberMute     :: Bool
      } deriving Show

instance FromJSON GuildMember where
  parseJSON = withObject "GuildMember" $ \o ->
    GuildMember <$> o .:  "user"
                <*> o .:? "nick"
                <*> o .:  "roles"
                <*> o .:  "joined_at"
                <*> o .:  "deaf"
                <*> o .:  "mute"


-- https://discordapp.com/developers/docs/resources/guild#guild-object

-- | Guilds in Discord represent a collection of users and channels into an isolated
--   "Server"
data Guild = Guild
      { guildId                  :: !Snowflake       -- ^ Gulid id
      , guildName                ::  String          -- ^ Guild name (2 - 100 chars)
      , guildIcon                ::  Maybe String    -- ^ Icon hash
      , guildSplash              ::  Maybe String    -- ^ Splash hash
      , guildOwnerId             :: !Snowflake       -- ^ Guild owner id
      , guildPermissions         ::  Maybe Integer
      , guildRegion              ::  String          -- ^ Guild voice region
      , guildAfkId               ::  Maybe Snowflake -- ^ Id of afk channel
      , guildAfkTimeout          :: !Integer         -- ^ Afk timeout in seconds
      , guildEmbedEnabled        ::  Maybe Bool      -- ^ Id of embedded channel
      , guildEmbedChannel        ::  Maybe Snowflake -- ^ Id of embedded channel
      , guildVerificationLevel   :: !Integer         -- ^ Level of verification
      , guildNotification        :: !Integer         -- ^ Level of default notifications
      , guildExplicitFilterLevel :: !Integer
      , guildRoles               :: [Role]           -- ^ Array of 'Role' objects
      , guildEmojis              :: [Emoji]          -- ^ Array of 'Emoji' objects
      , guildFeatures            :: [String]
      , guildMultiFactAuth       :: !Integer
      , guildApplicationId       ::  Maybe Snowflake
      } deriving Show

instance FromJSON Guild where
  parseJSON = withObject "Guild" $ \o ->
    Guild <$> o .:  "id"
          <*> o .:  "name"
          <*> o .:? "icon"
          <*> o .:? "splash"
          <*> o .:  "owner_id"
          <*> o .:? "permissions"
          <*> o .:  "region"
          <*> o .:? "afk_channel_id"
          <*> o .:  "afk_timeout"
          <*> o .:? "embed_enabled"
          <*> o .:? "embed_channel_id"
          <*> o .:  "verification_level"
          <*> o .:  "default_message_notifications"
          <*> o .:  "explicit_content_filter"
          <*> o .:  "roles"
          <*> o .:  "emojis"
          <*> o .:  "features"
          <*> o .:  "mfa_level"
          <*> o .:? "application_id"

data Unavailable = Unavailable
      { idOnceAvailable :: !Snowflake
      } deriving Show

instance FromJSON Unavailable where
  parseJSON = withObject "Unavailable" $ \o ->
       Unavailable <$> o .: "id"

data GuildInfo = GuildInfo
      { guildJoinedAt    :: UTCTime
      , guildLarge       :: Bool
      , guildMemberCount :: Integer
   -- , guildVoiceStates :: [VoiceState]  -- (without guildid) todo have to add voice state data type
      , guildMembers     :: [GuildMember]
      , guildChannels    :: [Channel]     -- ^ Channels in the guild (sent in GuildCreate)
   -- , guildPresences   :: [Presence]
      } deriving Show

instance FromJSON GuildInfo where
  parseJSON = withObject "GuildInfo" $ \o ->
    GuildInfo <$> o .: "joined_at"
              <*> o .: "large"
              <*> o .: "member_count"
           -- <*> o .: "voice_states"
              <*> o .: "members"
              <*> o .: "channels"

-- | Represents an emoticon (emoji)
data Emoji = Emoji
  { emojiId      :: Maybe Snowflake   -- ^ The emoji id
  , emojiName    :: String            -- ^ The emoji name
  , emojiRoles   :: Maybe [Snowflake] -- ^ Roles the emoji is active for
  , emojiManaged :: Maybe Bool        -- ^ Whether this emoji is managed
  } deriving (Show)

instance FromJSON Emoji where
  parseJSON = withObject "Emoji" $ \o ->
    Emoji <$> o .:  "id"
          <*> o .:  "name"
          <*> o .:? "roles"
          <*> o .:? "managed"

-- | Roles represent a set of permissions attached to a group of users. Roles have unique
--   names, colors, and can be "pinned" to the side bar, causing their members to be listed separately.
--   Roles are unique per guild, and can have separate permission profiles for the global context
--   (guild) and channel context.
data Role =
    Role {
        roleID      :: !Snowflake -- ^ The role id
      , roleName    :: String                    -- ^ The role name
      , roleColor   :: Integer                   -- ^ Integer representation of color code
      , roleHoist   :: Bool                      -- ^ If the role is pinned in the user listing
      , rolePos     :: Integer                   -- ^ Position of this role
      , rolePerms   :: Integer                   -- ^ Permission bit set
      , roleManaged :: Bool                      -- ^ Whether this role is managed by an integration
      , roleMention :: Bool                      -- ^ Whether this role is mentionable
    } deriving (Show, Eq)

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
data VoiceRegion =
    VoiceRegion
      { regionId          :: !Snowflake -- ^ Unique id of the region
      , regionName        :: String                    -- ^ Name of the region
      , regionHostname    :: String                    -- ^ Example hostname for the region
      , regionPort        :: Int                       -- ^ Example port for the region
      , regionVip         :: Bool                      -- ^ True if this is a VIP only server
      , regionOptimal     :: Bool                      -- ^ True for the closest server to a client
      , regionDepreciated :: Bool                      -- ^ Whether this is a deprecated region
      , regionCustom      :: Bool                      -- ^ Whether this is a custom region
      } deriving (Show)

instance FromJSON VoiceRegion where
  parseJSON = withObject "VoiceRegion" $ \o ->
    VoiceRegion <$> o .: "id"
                <*> o .: "name"
                <*> o .: "sample_hostname"
                <*> o .: "sample_port"
                <*> o .: "vip"
                <*> o .: "optimal"
                <*> o .: "deprecated"
                <*> o .: "custom"

-- | Represents a code to add a user to a guild
data Invite = Invite
      { inviteCode  ::  String    -- ^ The invite code
      , inviteGuild :: !Snowflake -- ^ The guild the code will invite to
      , inviteChan  :: !Snowflake -- ^ The channel the code will invite to
      }

instance FromJSON Invite where
  parseJSON = withObject "Invite" $ \o ->
    Invite <$>  o .: "code"
           <*> ((o .: "guild")   >>= (.: "id"))
           <*> ((o .: "channel") >>= (.: "id"))

-- | Invite code with additional metadata
data InviteWithMeta = InviteWithMeta Invite InviteMeta

instance FromJSON InviteWithMeta where
  parseJSON ob = InviteWithMeta <$> parseJSON ob <*> parseJSON ob

-- | Additional metadata about an invite.
data InviteMeta =
  InviteMeta {
      inviteCreator :: User    -- ^ The user that created the invite
    , inviteUses    :: Integer -- ^ Number of times the invite has been used
    , inviteMax     :: Integer -- ^ Max number of times the invite can be used
    , inviteAge     :: Integer -- ^ The duration (in seconds) after which the invite expires
    , inviteTemp    :: Bool    -- ^ Whether this invite only grants temporary membership
    , inviteCreated :: UTCTime -- ^ When the invite was created
    , inviteRevoked :: Bool    -- ^ If the invite is revoked
  }

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
data Integration =
    Integration
      { integrationId       :: !Snowflake -- ^ Integration id
      , integrationName     :: String                    -- ^ Integration name
      , integrationType     :: String                    -- ^ Integration type (Twitch, Youtube, ect.)
      , integrationEnabled  :: Bool                      -- ^ Is the integration enabled
      , integrationSyncing  :: Bool                      -- ^ Is the integration syncing
      , integrationRole     :: Snowflake                 -- ^ Id the integration uses for "subscribers"
      , integrationBehavior :: Integer                   -- ^ The behavior of expiring subscribers
      , integrationGrace    :: Integer                   -- ^ The grace period before expiring subscribers
      , integrationOwner    :: User                      -- ^ The user of the integration
      , integrationAccount  :: IntegrationAccount        -- ^ The account the integration links to
      , integrationSync     :: UTCTime                   -- ^ When the integration was last synced
      } deriving (Show)

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
data IntegrationAccount =
  Account
    { accountId   :: String -- ^ The id of the account.
    , accountName :: String -- ^ The name of the account.
    } deriving (Show)

instance FromJSON IntegrationAccount where
  parseJSON = withObject "Account" $ \o ->
    Account <$> o .: "id" <*> o .: "name"

-- | Represents an image to be used in third party sites to link to a discord channel
data GuildEmbed =
    GuildEmbed
      { embedEnabled :: !Bool      -- ^ Whether the embed is enabled
      , embedChannel :: !Snowflake -- ^ The embed channel id
      }

instance FromJSON GuildEmbed where
  parseJSON = withObject "GuildEmbed" $ \o -> GuildEmbed <$> o .: "enabled"
                                                         <*> o .: "snowflake"

instance ToJSON GuildEmbed where
  toJSON (GuildEmbed enabled snowflake) = object
    [ "enabled"   .= enabled
    , "snowflake" .= snowflake
    ]
