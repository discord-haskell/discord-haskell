{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Types relating to Discord Guilds (servers)
module Discord.Internal.Types.Guild where

import Data.Aeson
import Data.Data (Data)
import Data.Default (Default (..))
import qualified Data.Text as T
import Data.Time.Clock
import Discord.Internal.Types.Channel (Channel)
import Discord.Internal.Types.Color (DiscordColor)
import Discord.Internal.Types.Emoji (Emoji, StickerItem)
import Discord.Internal.Types.Prelude
import Discord.Internal.Types.User (GuildMember, User)

-- | Guilds in Discord represent a collection of users and channels into an isolated
--   "Server"
--
-- https://discord.com/developers/docs/resources/guild#guild-object
data Guild = Guild
  { -- | Gulid id
    guildId :: GuildId,
    -- | Guild name (2 - 100 chars)
    guildName :: T.Text,
    -- | Icon hash
    guildIcon :: Maybe T.Text,
    -- | Icon hash, when returned in template object
    guildIconHash :: Maybe T.Text,
    -- | Splash hash
    guildSplash :: Maybe T.Text,
    -- | Discovery splash hash
    guildDiscoverySplash :: Maybe T.Text,
    -- | True is user is the owner of the guild
    guildOwner :: Maybe Bool,
    -- | Guild owner id
    guildOwnerId :: UserId,
    -- | Total permissions for the user in the guild
    guildPermissions :: Maybe T.Text,
    -- | Id of afk channel
    guildAfkId :: Maybe ChannelId,
    -- | Afk timeout in seconds
    guildAfkTimeout :: Integer,
    -- | Id of embedded channel
    guildWidgetEnabled :: Maybe Bool,
    -- | Id of embedded channel
    guildWidgetChannelId :: Maybe ChannelId,
    -- | Level of verification
    guildVerificationLevel :: Integer,
    -- | Level of default notifications
    guildNotification :: Integer,
    -- | Whose media gets scanned
    guildExplicitFilterLevel :: Integer,
    -- | Array of 'Role' objects
    guildRoles :: [Role],
    -- | Array of 'Emoji' objects
    guildEmojis :: [Emoji],
    -- | Array of guild feature strings
    guildFeatures :: [T.Text],
    -- | MFA level for the guild
    guildMultiFactAuth :: !Integer,
    -- | Application id of the guild if bot created
    guildApplicationId :: Maybe ApplicationId,
    -- | Channel where guild notices such as welcome messages and boost events
    guildSystemChannelId :: Maybe ChannelId,
    -- | Flags on the system channel
    guildSystemChannelFlags :: Integer,
    -- | Id of channel with rules/guidelines
    guildRulesChannelId :: Maybe ChannelId,
    -- | When this guild was joined at
    guildJoinedAt :: Maybe UTCTime,
    -- | True if this guild is considered large
    guildLarge :: Maybe Bool,
    -- | True if the guild is unavailable due to outage
    guildUnavailable :: Maybe Bool,
    -- | Total number of members in the guild
    -- voice_states
    guildMemberCount :: Maybe Integer,
    -- | Users in the guild
    guildMembers :: Maybe [GuildMember],
    -- | Channels in the guild
    guildChannels :: Maybe [Channel],
    -- | All active threads in the guild that the current user has permission to view
    guildThreads :: Maybe [Channel],
    -- | Presences of the members in the guild
    guildPresences :: Maybe [PresenceInfo],
    -- | Maximum number of prescences in the guild
    guildMaxPresences :: Maybe Integer,
    -- | Maximum number of members in the guild
    guildMaxMembers :: Maybe Integer,
    -- | Vanity url code for the guild
    guildVanityURL :: Maybe T.Text,
    -- | Description of a commmunity guild
    guildDescription :: Maybe T.Text,
    -- | Banner hash
    guildBanner :: Maybe T.Text,
    -- | Premium tier (boost level)
    guildPremiumTier :: Integer,
    -- | Number of boosts the guild has
    guildSubscriptionCount :: Maybe Integer,
    -- | Preferred locale of a community server
    guildPreferredLocale :: T.Text,
    -- | Id of channel where admins and mods get updates
    guildPublicUpdatesChannel :: Maybe ChannelId,
    -- | Maximum number of users in video channel
    guildMaxVideoUsers :: Maybe Integer,
    -- | Approximate number of members in the guild
    guildApproxMemberCount :: Maybe Integer,
    -- | Approximate number of non-offline members in the guild
    -- welcome_screen
    guildApproxPresenceCount :: Maybe Integer,
    -- | Guild NSFW level
    -- stage_instances
    guildNSFWLevel :: Integer,
    -- | Custom guild stickers
    -- guild_scheduled_events
    guildStickers :: Maybe [StickerItem],
    -- | Whether the guild has the boost progress bar enabled
    guildPremiumBar :: Bool
  }
  deriving (Show, Read, Eq, Ord)

instance FromJSON Guild where
  parseJSON = withObject "Guild" $ \o ->
    Guild <$> o .: "id"
      <*> o .: "name"
      <*> o .:? "icon"
      <*> o .:? "icon_hash"
      <*> o .:? "splash"
      <*> o .:? "discovery_splash"
      <*> o .:? "owner"
      <*> o .: "owner_id"
      <*> o .:? "permissions"
      <*> o .:? "afk_channel_id"
      <*> o .: "afk_timeout"
      <*> o .:? "widget_enabled"
      <*> o .:? "widget_channel_id"
      <*> o .: "verification_level"
      <*> o .: "default_message_notifications"
      <*> o .: "explicit_content_filter"
      <*> o .: "roles"
      <*> o .: "emojis"
      <*> o .: "features"
      <*> o .: "mfa_level"
      <*> o .:? "application_id"
      <*> o .:? "system_channel_id"
      <*> o .: "system_channel_flags"
      <*> o .:? "rules_channel_id"
      <*> o .:? "joined_at"
      <*> o .:? "large"
      <*> o .:? "unavailable"
      <*> o .:? "member_count"
      -- voice_states
      <*> o .:? "members"
      <*> o .:? "channels"
      <*> o .:? "threads"
      <*> o .:? "presences"
      <*> o .:? "max_presences"
      <*> o .:? "max_members"
      <*> o .:? "vanity_url_code"
      <*> o .:? "description"
      <*> o .:? "banner"
      <*> o .: "premium_tier"
      <*> o .:? "premium_subscription_count"
      <*> o .: "preferred_locale"
      <*> o .:? "public_updates_channel_id"
      <*> o .:? "max_video_channel_users"
      <*> o .:? "approximate_member_count"
      <*> o .:? "approximate_presence_count"
      -- welcome_screen
      <*> o .: "nsfw_level"
      -- stage_instances
      <*> o .:? "stickers"
      <*> o .: "premium_progress_bar_enabled"

newtype GuildUnavailable = GuildUnavailable
  { idOnceAvailable :: GuildId
  }
  deriving (Show, Read, Eq, Ord)

instance FromJSON GuildUnavailable where
  parseJSON = withObject "GuildUnavailable" $ \o ->
    GuildUnavailable <$> o .: "id"

data PresenceInfo = PresenceInfo
  { presenceUserId :: UserId,
    -- , presenceRoles   :: [RoleId]
    presenceActivities :: Maybe [Activity],
    presenceGuildId :: Maybe GuildId,
    presenceStatus :: T.Text
  }
  deriving (Show, Read, Eq, Ord)

instance FromJSON PresenceInfo where
  parseJSON = withObject "PresenceInfo" $ \o ->
    PresenceInfo <$> (o .: "user" >>= (.: "id"))
      <*> o .: "activities"
      <*> o .:? "guild_id"
      <*> o .: "status"

-- | Object for a single activity
--
-- https://discord.com/developers/docs/topics/gateway#activity-object
--
-- When setting a bot's activity, only the name, url, and type are sent - and
-- it seems that not many types are permitted either.
data Activity = Activity
  { -- | Name of activity
    activityName :: T.Text,
    -- | Type of activity
    activityType :: ActivityType,
    -- | URL of the activity (only verified when streaming)
    activityUrl :: Maybe T.Text,
    -- | unix time in milliseconds
    activityCreatedAt :: Integer,
    -- | Start and end times
    activityTimeStamps :: Maybe ActivityTimestamps,
    -- | Application of the activity
    activityApplicationId :: Maybe ApplicationId,
    -- | Details of Activity
    activityDetails :: Maybe T.Text,
    -- | State of the user's party
    activityState :: Maybe T.Text,
    -- | Simplified emoji object
    activityEmoji :: Maybe Emoji,
    -- | Info for the current player's party
    -- assets
    -- secrets
    activityParty :: Maybe ActivityParty,
    -- | Whether or not the activity is an instanced game session
    activityInstance :: Maybe Bool,
    -- | The flags https://discord.com/developers/docs/topics/gateway#activity-object-activity-flags
    activityFlags :: Maybe Integer,
    -- | Custom buttons shown in Rich Presence
    activityButtons :: Maybe [ActivityButton]
  }
  deriving (Show, Read, Eq, Ord)

instance Default Activity where
  def = Activity "discord-haskell" ActivityTypeGame Nothing 0 Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

instance FromJSON Activity where
  parseJSON = withObject "Activity" $ \o -> do
    Activity <$> o .: "name"
      <*> o .: "type"
      <*> o .:? "url"
      <*> o .: "created_at"
      <*> o .:? "timestamps"
      <*> o .:? "application_id"
      <*> o .:? "details"
      <*> o .:? "state"
      <*> o .:? "emoji"
      <*> o .:? "party"
      -- assets
      -- secrets
      <*> o .:? "instance"
      <*> o .:? "flags"
      <*> o .:? "buttons"

data ActivityTimestamps = ActivityTimestamps
  { -- | unix time in milliseconds
    activityTimestampsStart :: Maybe Integer,
    -- | unix time in milliseconds
    activityTimestampsEnd :: Maybe Integer
  }
  deriving (Show, Read, Eq, Ord)

instance FromJSON ActivityTimestamps where
  parseJSON = withObject "ActivityTimestamps" $ \o ->
    ActivityTimestamps <$> o .:? "start"
      <*> o .:? "end"

data ActivityParty = ActivityParty
  { activityPartyId :: Maybe T.Text,
    activityPartySize :: Maybe (Integer, Integer)
  }
  deriving (Show, Read, Eq, Ord)

instance FromJSON ActivityParty where
  parseJSON = withObject "ActivityParty" $ \o ->
    ActivityParty <$> o .:? "id"
      <*> o .:? "size"

data ActivityButton = ActivityButton
  { activityButtonLabel :: T.Text,
    activityButtonUrl :: T.Text
  }
  deriving (Show, Read, Eq, Ord)

instance FromJSON ActivityButton where
  parseJSON = withObject "ActivityButton" $ \o ->
    ActivityButton <$> o .: "label"
      <*> o .: "url"

-- | To see what these look like, go to here:
-- https://discord.com/developers/docs/topics/gateway#activity-object-activity-types
data ActivityType
  = ActivityTypeGame
  | ActivityTypeStreaming
  | ActivityTypeListening
  | ActivityTypeWatching
  | ActivityTypeCustom
  | ActivityTypeCompeting
  deriving (Show, Read, Eq, Ord, Data)

instance InternalDiscordEnum ActivityType where
  discordTypeStartValue = ActivityTypeGame
  fromDiscordType ActivityTypeGame = 0
  fromDiscordType ActivityTypeStreaming = 1
  fromDiscordType ActivityTypeListening = 2
  fromDiscordType ActivityTypeWatching = 3
  fromDiscordType ActivityTypeCustom = 4
  fromDiscordType ActivityTypeCompeting = 5

instance FromJSON ActivityType where
  parseJSON = discordTypeParseJSON "ActivityType"

data PartialGuild = PartialGuild
  { partialGuildId :: GuildId,
    partialGuildName :: T.Text,
    partialGuildIcon :: Maybe T.Text,
    partialGuildOwner :: Bool,
    partialGuildPermissions :: T.Text
  }
  deriving (Show, Read, Eq, Ord)

instance FromJSON PartialGuild where
  parseJSON = withObject "PartialGuild" $ \o ->
    PartialGuild <$> o .: "id"
      <*> o .: "name"
      <*> o .:? "icon"
      <*> o .:? "owner" .!= False
      <*> o .: "permissions"

-- | Roles represent a set of permissions attached to a group of users. Roles have unique
--   names, colors, and can be "pinned" to the side bar, causing their members to be listed separately.
--   Roles are unique per guild, and can have separate permission profiles for the global context
--   (guild) and channel context.
data Role = Role
  { -- | The role id
    roleId :: RoleId,
    -- | The role name
    roleName :: T.Text,
    -- | Integer representation of color code
    roleColor :: DiscordColor,
    -- | If the role is pinned in the user listing
    roleHoist :: Bool,
    -- | Position of this role
    rolePos :: Integer,
    -- | Permission bit set
    rolePerms :: T.Text,
    -- | Whether this role is managed by an integration
    roleManaged :: Bool,
    -- | Whether this role is mentionable
    roleMention :: Bool
  }
  deriving (Show, Read, Eq, Ord)

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
  { -- | Unique id of the region
    voiceRegionId :: T.Text,
    -- | Name of the region
    voiceRegionName :: T.Text,
    -- | True if this is a VIP only server
    voiceRegionVip :: Bool,
    -- | True for the closest server to a client
    voiceRegionOptimal :: Bool,
    -- | Whether this is a deprecated region
    voiceRegionDeprecated :: Bool,
    -- | Whether this is a custom region
    voiceRegionCustom :: Bool
  }
  deriving (Show, Read, Eq, Ord)

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
  { guildBanReason :: T.Text,
    guildBanUser :: User
  }
  deriving (Show, Read, Eq, Ord)

instance FromJSON GuildBan where
  parseJSON = withObject "GuildBan" $ \o -> GuildBan <$> o .: "reason" <*> o .: "user"

-- | Represents a code to add a user to a guild
data Invite = Invite
  { -- | The invite code
    inviteCode :: T.Text,
    -- | The guild the code will invite to
    inviteGuildId :: Maybe GuildId,
    -- | The channel the code will invite to
    inviteChannelId :: ChannelId
  }
  deriving (Show, Read, Eq, Ord)

instance FromJSON Invite where
  parseJSON = withObject "Invite" $ \o ->
    Invite <$> o .: "code"
      <*> ( do
              g <- o .:? "guild"
              case g of
                Just g2 -> g2 .: "id"
                Nothing -> pure Nothing
          )
      <*> ((o .: "channel") >>= (.: "id"))

-- | Invite code with additional metadata
data InviteWithMeta = InviteWithMeta Invite InviteMeta

instance FromJSON InviteWithMeta where
  parseJSON ob = InviteWithMeta <$> parseJSON ob <*> parseJSON ob

-- | Additional metadata about an invite.
data InviteMeta = InviteMeta
  { -- | The user that created the invite
    inviteCreator :: User,
    -- | Number of times the invite has been used
    inviteUses :: Integer,
    -- | Max number of times the invite can be used
    inviteMax :: Integer,
    -- | The duration (in seconds) after which the invite expires
    inviteAge :: Integer,
    -- | Whether this invite only grants temporary membership
    inviteTemp :: Bool,
    -- | When the invite was created
    inviteCreated :: UTCTime,
    -- | If the invite is revoked
    inviteRevoked :: Bool
  }
  deriving (Show, Read, Eq, Ord)

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
  { -- | Integration id
    integrationId :: !Snowflake,
    -- | Integration name
    integrationName :: T.Text,
    -- | Integration type (Twitch, Youtube, ect.)
    integrationType :: T.Text,
    -- | Is the integration enabled
    integrationEnabled :: Bool,
    -- | Is the integration syncing
    integrationSyncing :: Bool,
    -- | Id the integration uses for "subscribers"
    integrationRole :: RoleId,
    -- | The behavior of expiring subscribers
    integrationBehavior :: Integer,
    -- | The grace period before expiring subscribers
    integrationGrace :: Integer,
    -- | The user of the integration
    integrationOwner :: User,
    -- | The account the integration links to
    integrationAccount :: IntegrationAccount,
    -- | When the integration was last synced
    integrationSync :: UTCTime
  }
  deriving (Show, Read, Eq, Ord)

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
  { -- | The id of the account.
    accountId :: T.Text,
    -- | The name of the account.
    accountName :: T.Text
  }
  deriving (Show, Read, Eq, Ord)

instance FromJSON IntegrationAccount where
  parseJSON = withObject "IntegrationAccount" $ \o ->
    IntegrationAccount <$> o .: "id" <*> o .: "name"

-- | Represents an image to be used in third party sites to link to a discord channel
data GuildWidget = GuildWidget
  { -- | Whether the widget is enabled
    widgetEnabled :: Bool,
    -- | The widget channel id
    widgetChannelId :: ChannelId
  }
  deriving (Show, Read, Eq, Ord)

instance FromJSON GuildWidget where
  parseJSON = withObject "GuildWidget" $ \o ->
    GuildWidget <$> o .: "enabled" <*> o .: "channel_id"

instance ToJSON GuildWidget where
  toJSON (GuildWidget enabled snowflake) =
    object
      [ "enabled" .= enabled,
        "channel_id" .= snowflake
      ]
