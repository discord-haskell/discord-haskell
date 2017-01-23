{-# LANGUAGE OverloadedStrings #-}
module Network.Discord.Types.Guild where
  import Data.Time.Clock

  import Data.Aeson
  import Control.Monad (mzero)
  import Control.Applicative ((<|>))

  import Network.Discord.Types.Channel
  import Network.Discord.Types.Prelude

  -- |Representation of a guild member.
  data Member = GuildMember Snowflake User
            | MemberShort User (Maybe String) [Snowflake]
            deriving Show
  instance FromJSON Member where
    parseJSON (Object o) =
      GuildMember <$> o .: "guild_id" <*> o .: "user"
    parseJSON _ = mzero

  -- |Temporary representation of a guild, will be replaced
  data Guild =
    Guild Snowflake
    deriving Show
  instance FromJSON Guild where
    parseJSON (Object o) = Guild <$> o .: "id"
    parseJSON _          = mzero

  data Role = 
      Role {
          roleID      :: Snowflake
        , roleName    :: String
        , roleColor   :: Integer
        , roleHoist   :: Bool
        , rolePos     :: Integer
        , rolePerms   :: Integer
        , roleManaged :: Bool
        , roleMention :: Bool
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
      VoiceRegion {
          regionId          :: Snowflake
        , regionName        :: String
        , regionHostname    :: String
        , regionPort        :: Int
        , regionVip         :: Bool
        , regionOptimal     :: Bool
        , regionDepreciated :: Bool
        , regionCustom      :: Bool
      }
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

  data Invite =
      Invite {
          inviteCode  :: String
        , inviteGuild :: InviteGuild
        , inviteChann :: InviteChannel
      }
    | InviteLong Invite InviteMeta

  instance FromJSON Invite where
    parseJSON ob@(Object o) =
          InviteLong <$> parseJSON ob <*> parseJSON ob
      <|> Invite
          <$> o .: "code"
          <*> o .: "guild"
          <*> o .: "channel"
    parseJSON _ = mzero

  data InviteMeta =
    InviteMeta {
        inviteCreator :: User
      , inviteUses    :: Integer
      , inviteMax     :: Integer
      , inviteAge     :: Integer
      , inviteTemp    :: Bool
      , inviteCreated :: UTCTime
      , inviteRevoked :: Bool
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

  data InviteGuild =
    InviteGuild {
        guildId     :: Snowflake
      , guildName   :: String
      , guildSplash :: String
      , guildIcon   :: String
    }

  instance FromJSON InviteGuild where
    parseJSON (Object o) = InviteGuild
      <$> o .: "id"
      <*> o .: "name"
      <*> o .: "splash"
      <*> o .: "icon"
    parseJSON _ = mzero

  data InviteChannel =
    InviteChannel {
        inviteChannel :: Snowflake
      , inviteName    :: String
      , inviteType    :: String
    }

  instance FromJSON InviteChannel where
    parseJSON (Object o) = InviteChannel
      <$> o .: "id"
      <*> o .: "name"
      <*> o .: "type"
    parseJSON _ = mzero

  data Integration =
      Integration {
          integrationId       :: Snowflake
        , integrationName     :: String
        , integrationType     :: String
        , integrationEnabled  :: Bool
        , integrationSyncing  :: Bool
        , integrationRole     :: Snowflake
        , integrationBehavior :: Integer
        , integrationGrace    :: Integer
        , integrationOwner    :: User
        , integrationAccount  :: IntegrationAccount
        , integrationSync     :: UTCTime
      }

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

  data IntegrationAccount =
    Account {
        accountId   :: String
      , accountName :: String
    }

  instance FromJSON IntegrationAccount where
    parseJSON (Object o) = Account
      <$> o .: "id"
      <*> o .: "name"
    parseJSON _ = mzero

  data GuildEmbed =
      GuildEmbed {
          embedEnabled :: Bool
        , embedChannel :: Snowflake
      }
  instance FromJSON GuildEmbed where
    parseJSON (Object o) = GuildEmbed
      <$> o .: "enabled"
      <*> o .: "snowflake"
    parseJSON _ = mzero

  instance ToJSON GuildEmbed where
    toJSON (GuildEmbed enabled snowflake) = object [
        "enabled" .= enabled
      , "snowflake" .= snowflake
      ]
