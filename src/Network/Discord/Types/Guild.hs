{-# LANGUAGE OverloadedStrings #-}
-- | Types relating to Discord Guilds (servers)
module Network.Discord.Types.Guild where
  import Data.Time.Clock

  import Data.Aeson
  import Control.Applicative ((<|>))
  import Control.Monad (mzero)

  import Network.Discord.Types.Channel
  import Network.Discord.Types.Prelude

  -- |Representation of a guild member.
  data Member = GuildMember {-# UNPACK #-} !Snowflake User
            | MemberShort User (Maybe String) ![Snowflake]
            deriving Show
  instance FromJSON Member where
    parseJSON (Object o) =
      GuildMember <$> o .: "guild_id" <*> o .: "user"
    parseJSON _ = mzero

  -- | Guilds in Discord represent a collection of users and channels into an isolated
  --   "Server"
  data Guild
    = Guild 
        { guildId           :: {-# UNPACK #-} !Snowflake -- ^ Gulid id
        , guildName         ::                 String    -- ^ Guild name (2 - 100 chars)
        , guildIcon         ::                 String    -- ^ Icon hash
        , guildSplash       ::                 String    -- ^ Splash hash
        , guildOwner        ::                !Snowflake -- ^ Guild owner id
        , guildRegion       ::                 String    -- ^ Guild voice region
        , guildAfkId        ::                !Snowflake -- ^ Id of afk channel
        , guildAfkTimeout   ::                !Integer   -- ^ Afk timeout in seconds
        , guildEmbedEnabled ::                !Bool      -- ^ Is this guild embeddable?
        , guildEmbedChannel ::                !Snowflake -- ^ Id of embedded channel
        , guildVerification ::                !Integer   -- ^ Level of verification
        , guildNotification ::                !Integer   -- ^ Level of default notifications
        , guildRoles        ::                [Role]     -- ^ Array of 'Role' objects
        , guildEmojis       ::                [Emoji]    -- ^ Array of 'Emoji' objects
        }
    | Unavailable 
        { guildId :: {-# UNPACK #-} !Snowflake
        } deriving Show

  instance FromJSON Guild where
    parseJSON (Object o) = do
      short <- o .:? "unavailable" .!= False
      if short
        then Unavailable <$> o .: "id"
        else
          Guild <$> o .:  "id"
                <*> o .:  "name"
                <*> o .:? "icon" .!= ""
                <*> o .:? "hash" .!= ""
                <*> o .:  "owner_id"
                <*> o .:  "region"
                <*> o .:? "afk_channel_id"   .!= 0
                <*> o .:? "afk_timeout"      .!= 0
                <*> o .:? "embed_enabled"    .!= False
                <*> o .:? "embed_channel_id" .!= 0
                <*> o .:  "verification_level"
                <*> o .:  "default_message_notifications"
                <*> o .:  "roles"
                <*> o .:  "emojis"
    parseJSON _          = mzero

  -- | Represents an emoticon (emoji)
  data Emoji = Emoji
    { emojiId      :: {-# UNPACK #-} !Snowflake   -- ^ The emoji id
    , emojiName    ::                 String      -- ^ The emoji name
    , emojiRoles   ::                ![Snowflake] -- ^ Roles the emoji is active for
    , emojiManaged ::                !Bool        -- ^ Whether this emoji is managed
    } deriving (Show)

  instance FromJSON Emoji where
    parseJSON (Object o) =
      Emoji <$> o .: "id"
            <*> o .: "name"
            <*> o .: "roles"
            <*> o .: "managed"
    parseJSON _ = mzero

  -- | Roles represent a set of permissions attached to a group of users. Roles have unique
  --   names, colors, and can be "pinned" to the side bar, causing their members to be listed separately.
  --   Roles are unique per guild, and can have separate permission profiles for the global context
  --   (guild) and channel context.
  data Role = 
      Role {
          roleID      :: {-# UNPACK #-} !Snowflake -- ^ The role id
        , roleName    :: String                    -- ^ The role name
        , roleColor   :: Integer                   -- ^ Integer representation of color code
        , roleHoist   :: Bool                      -- ^ If the role is pinned in the user listing
        , rolePos     :: Integer                   -- ^ Position of this role
        , rolePerms   :: Integer                   -- ^ Permission bit set
        , roleManaged :: Bool                      -- ^ Whether this role is managed by an integration
        , roleMention :: Bool                      -- ^ Whether this role is mentionable
      } deriving (Show, Eq)

  instance FromJSON Role where
    parseJSON (Object o) = Role
      <$> o .: "id"
      <*> o .: "name"
      <*> o .: "color"
      <*> o .: "hoist"
      <*> o .: "position"
      <*> o .: "permissions"
      <*> o .: "managed"
      <*> o .: "mentionable"
    parseJSON _ = mzero
  
  -- | VoiceRegion is only refrenced in Guild endpoints, will be moved when voice support is added
  data VoiceRegion =
      VoiceRegion
        { regionId          :: {-# UNPACK #-} !Snowflake -- ^ Unique id of the region
        , regionName        :: String                    -- ^ Name of the region
        , regionHostname    :: String                    -- ^ Example hostname for the region
        , regionPort        :: Int                       -- ^ Example port for the region
        , regionVip         :: Bool                      -- ^ True if this is a VIP only server
        , regionOptimal     :: Bool                      -- ^ True for the closest server to a client
        , regionDepreciated :: Bool                      -- ^ Whether this is a deprecated region
        , regionCustom      :: Bool                      -- ^ Whether this is a custom region
        } deriving (Show)

  instance FromJSON VoiceRegion where
    parseJSON (Object o) = VoiceRegion
      <$> o .: "id"
      <*> o .: "name"
      <*> o .: "sample_hostname"
      <*> o .: "sample_port"
      <*> o .: "vip"
      <*> o .: "optimal"
      <*> o .: "deprecated"
      <*> o .: "custom"
    parseJSON _ = mzero

  -- | Represents a code to add a user to a guild
  data Invite =
      Invite {
          inviteCode  ::  String    -- ^ The invite code
        , inviteGuild :: !Snowflake -- ^ The guild the code will invite to
        , inviteChan  :: !Snowflake -- ^ The channel the code will invite to
      }
    -- | Invite code with additional metadata
    | InviteLong Invite InviteMeta

  instance FromJSON Invite where
    parseJSON ob@(Object o) =
          InviteLong <$> parseJSON ob <*> parseJSON ob
      <|> Invite
          <$>  o .: "code"
          <*> ((o .: "guild")   >>= (.: "id"))
          <*> ((o .: "channel") >>= (.: "id"))
    parseJSON _ = mzero
  
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
    parseJSON (Object o) = InviteMeta
      <$> o .: "inviter"
      <*> o .: "uses"
      <*> o .: "max_uses"
      <*> o .: "max_age"
      <*> o .: "temporary"
      <*> o .: "created_at"
      <*> o .: "revoked"
    parseJSON _ = mzero

  -- | Represents the behavior of a third party account link.
  data Integration =
      Integration
        { integrationId       :: {-# UNPACK #-} !Snowflake -- ^ Integration id
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
    parseJSON (Object o) = Integration
      <$> o .: "id"
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
    parseJSON _ = mzero
  
  -- | Represents a third party account link.
  data IntegrationAccount =
    Account 
      { accountId   :: String -- ^ The id of the account.
      , accountName :: String -- ^ The name of the account.
      } deriving (Show)

  instance FromJSON IntegrationAccount where
    parseJSON (Object o) = Account
      <$> o .: "id"
      <*> o .: "name"
    parseJSON _ = mzero

  -- | Represents an image to be used in third party sites to link to a discord channel
  data GuildEmbed =
      GuildEmbed
        { embedEnabled :: !Bool                     -- ^ Whether the embed is enabled
        , embedChannel :: {-# UNPACK #-} !Snowflake -- ^ The embed channel id
        }
  instance FromJSON GuildEmbed where
    parseJSON (Object o) = GuildEmbed
      <$> o .: "enabled"
      <*> o .: "snowflake"
    parseJSON _ = mzero

  instance ToJSON GuildEmbed where
    toJSON (GuildEmbed enabled snowflake) = object
      [ "enabled"   .= enabled
      , "snowflake" .= snowflake
      ]
