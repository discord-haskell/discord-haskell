{-# LANGUAGE OverloadedStrings #-}

-- | Types relating to Discord Guilds (servers)
module Discord.Types.Guild where

import Data.Time.Clock

import Data.Aeson
import qualified Data.Text as T

import Discord.Types.Channel
import Discord.Types.Prelude

-- | Representation of a guild member.
data GuildMember = GuildMember
      { memberUser     :: User
      , memberNick     :: Maybe T.Text
      , memberRoles    :: [Snowflake]
      , memberJoinedAt :: UTCTime
      , memberDeaf     :: Bool
      , memberMute     :: Bool
      } deriving (Show, Eq, Ord)

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
      { guildId                  :: GuildId       -- ^ Gulid id
      , guildName                :: T.Text          -- ^ Guild name (2 - 100 chars)
      , guildIcon                :: Maybe T.Text    -- ^ Icon hash
      , guildSplash              :: Maybe T.Text    -- ^ Splash hash
      , guildOwnerId             :: UserId       -- ^ Guild owner id
      , guildPermissions         :: Maybe Integer
      , guildRegion              :: T.Text          -- ^ Guild voice region
      , guildAfkId               :: Maybe ChannelId -- ^ Id of afk channel
      , guildAfkTimeout          :: Integer         -- ^ Afk timeout in seconds
      , guildEmbedEnabled        :: Maybe Bool      -- ^ Id of embedded channel
      , guildEmbedChannel        :: Maybe ChannelId -- ^ Id of embedded channel
      , guildVerificationLevel   :: Integer         -- ^ Level of verification
      , guildNotification        :: Integer         -- ^ Level of default notifications
      , guildExplicitFilterLevel :: Integer
      , guildRoles               :: [Role]           -- ^ Array of 'Role' objects
      , guildEmojis              :: [Emoji]          -- ^ Array of 'Emoji' objects
      , guildFeatures            :: [T.Text]
      , guildMultiFactAuth       :: !Integer
      , guildApplicationId       :: Maybe Snowflake
      } deriving (Show, Eq, Ord)

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

data GuildUnavailable = GuildUnavailable
      { idOnceAvailable :: GuildId
      } deriving (Show, Eq, Ord)

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
      } deriving (Show, Eq, Ord)

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
      , partialGuildPermissions :: Integer
      } deriving (Show, Eq, Ord)

instance FromJSON PartialGuild where
  parseJSON = withObject "PartialGuild" $ \o ->
    PartialGuild <$> o .:  "id"
                 <*> o .:  "name"
                 <*> o .:? "icon"
                 <*> o .:?  "owner" .!= False
                 <*> o .:  "permissions"

-- | Represents an emoticon (emoji)
data Emoji = Emoji
  { emojiId      :: Maybe EmojiId   -- ^ The emoji id
  , emojiName    :: T.Text            -- ^ The emoji name
  , emojiRoles   :: Maybe [RoleId] -- ^ Roles the emoji is active for
  , emojiManaged :: Maybe Bool        -- ^ Whether this emoji is managed
  } deriving (Show, Eq, Ord)

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
        roleID      :: RoleId -- ^ The role id
      , roleName    :: T.Text                    -- ^ The role name
      , roleColor   :: Integer                   -- ^ Integer representation of color code
      , roleHoist   :: Bool                      -- ^ If the role is pinned in the user listing
      , rolePos     :: Integer                   -- ^ Position of this role
      , rolePerms   :: Integer                   -- ^ Permission bit set
      , roleManaged :: Bool                      -- ^ Whether this role is managed by an integration
      , roleMention :: Bool                      -- ^ Whether this role is mentionable
    } deriving (Show, Eq, Ord)

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
      , voiceRegionDepreciated :: Bool        -- ^ Whether this is a deprecated region
      , voiceRegionCustom      :: Bool        -- ^ Whether this is a custom region
      } deriving (Show, Eq, Ord)

instance FromJSON VoiceRegion where
  parseJSON = withObject "VoiceRegion" $ \o ->
    VoiceRegion <$> o .: "id"
                <*> o .: "name"
                <*> o .: "vip"
                <*> o .: "optimal"
                <*> o .: "deprecated"
                <*> o .: "custom"

-- | Info about a Ban
data Ban = Ban
      { banReason  :: T.Text
      , banUser    :: User
      } deriving (Show, Eq, Ord)

instance FromJSON Ban where
  parseJSON = withObject "Ban" $ \o -> Ban <$> o .: "reason" <*> o .: "user"

-- | Represents a code to add a user to a guild
data Invite = Invite
      { inviteCode  :: T.Text    -- ^ The invite code
      , inviteGuildId :: Maybe GuildId -- ^ The guild the code will invite to
      , inviteChannelId :: ChannelId -- ^ The channel the code will invite to
      } deriving (Show, Eq, Ord)

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
    } deriving (Show, Eq, Ord)

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
      } deriving (Show, Eq, Ord)

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
    } deriving (Show, Eq, Ord)

instance FromJSON IntegrationAccount where
  parseJSON = withObject "IntegrationAccount" $ \o ->
    IntegrationAccount <$> o .: "id" <*> o .: "name"

-- | Represents an image to be used in third party sites to link to a discord channel
data GuildEmbed = GuildEmbed
      { embedEnabled :: Bool      -- ^ Whether the embed is enabled
      , embedChannel :: ChannelId -- ^ The embed channel id
      } deriving (Show, Eq, Ord)

instance FromJSON GuildEmbed where
  parseJSON = withObject "GuildEmbed" $ \o ->
    GuildEmbed <$> o .: "enabled" <*> o .: "snowflake"

instance ToJSON GuildEmbed where
  toJSON (GuildEmbed enabled snowflake) = object
    [ "enabled"   .= enabled
    , "snowflake" .= snowflake
    ]
