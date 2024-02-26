{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Data structures pertaining to Discord User
module Discord.Internal.Types.ApplicationInfo where

import Data.Aeson
import qualified Data.Text as T
import Discord.Internal.Types.Prelude

-- | Structure containing partial information about an Application
--    <https://discord.com/developers/docs/resources/application#application-object>
data FullApplication = FullApplication
  { fullApplicationID :: ApplicationId
  , fullApplicationName :: T.Text
  , fullApplicationIcon :: Maybe T.Text -- ^ Icon hash
  , fullApplicationDescription :: T.Text
  , fullApplicationRPCOrigins :: Maybe [T.Text] -- ^ List of RPC origin URLs, if RPC is enabled
  , fullApplicationBotPublic :: Bool -- ^ When false, only the app owner can add the app to guilds
  , fullApplicationRequiresCodeGrant :: Bool -- ^ When true, the app's bot will only join upon completion of the full OAuth2 code grant flow
  , fullApplicationBotUserId :: Maybe UserId -- ^ UserId of the bot user associated with the app
  -- , fullApplicationBot :: User -- partial user, test what it has?
  , fullApplicationTermsOfServiceUrl :: Maybe T.Text
  , fullApplicationPrivacyPolicyUrl :: Maybe T.Text
  , fullApplicationOwnerId :: Maybe UserId -- ^ UserId for the bot user associated with the app
  -- , fullApplicationOwner :: User -- ^ partial user, test what it has?
  , fullApplicationVerifyKey :: T.Text -- ^ Hex encoded key for verification in interactions and the GameSDK's GetTicket
  , fullApplicationTeam :: Maybe DiscordTeam -- ^ Included if the app belongs to a team
  , fullApplicationGuildId :: Maybe GuildId -- ^ Guild ID associated with the App. 
  -- fullapplicationGuild :: PartialGuild -- ^ Don't include this because it's partial
  , fullApplicationGameSKUId :: Maybe GameSKUId
  , fullapplicationSlug  :: Maybe T.Text -- ^ If this app is a game sold on Discord, this field will be the URL slug that links to the store page
  , fullApplicationCoverImage :: Maybe T.Text -- ^ App's default rich presence invite cover image hash
  , fullApplicationFlags :: Maybe Integer -- ^ App's public flags
  , fullApplicationApproximateGuildCount :: Maybe Integer -- ^ Approximate count of guilds the app has been added to
  , fullApplicationRedirectURIs :: Maybe [T.Text] -- ^ Array of redirect URIs for the app
  , fullApplicationInteractionEndpointURL :: Maybe T.Text -- ^ Interactions endpoint URL for the app
  , fullApplicationRoleConnectionVerificationURL :: Maybe T.Text -- ^ Role connection verification URL for the app
  , fullApplicationTags :: Maybe [T.Text] -- ^ List of tags describing the content and functionality of the app. Max of 5 tags.
  , fullApplicationInstallParams :: Maybe DiscordInstallParams
  , fullApplicationCustomInstallUrl :: Maybe T.Text -- ^ Default custom authorization URL for the app, if enabled
  } deriving (Show, Read, Eq, Ord)


instance FromJSON FullApplication where
  parseJSON = withObject "FullApplication" $ \o ->
    FullApplication <$> o .: "id"
                    <*> o .: "name"
                    <*> o .:? "icon"
                    <*> o .: "description"
                    <*> o .:? "rpc_origins"
                    <*> o .: "bot_public"
                    <*> o .: "bot_require_code_grant"
                    <*> o .:? "bot" -- (o .:? "bot" >>= (\b -> b .: "id"))
                    <*> o .:? "terms_of_service_url"
                    <*> o .:? "privacy_policy_url"
                    <*> o .:? "owner" -- (o .:? "owner" >>= (\b -> b .: "id"))
                    <*> o .: "verify_key"
                    <*> o .:? "team"
                    <*> o .:? "guild_id"
                    -- <*> o .:? "guild"
                    <*> o .:? "primary_sku_id"
                    <*> o .:? "slug"
                    <*> o .:? "cover_image"
                    <*> o .:? "flags"
                    <*> o .:? "approximate_guild_count"
                    <*> o .:? "redirect_uris"
                    <*> o .:? "interactions_endpoint_url"
                    <*> o .:? "role_connections_verification_url"
                    <*> o .:? "tags"
                    <*> o .:? "install_params"
                    <*> o .:? "custom_install_url"


zz = undefined
zzb = undefined

data DiscordTeam = DiscordTeam
  { discordTeamIcon :: Maybe T.Text
  , discordTeamId :: DiscordTeamId
  , discordTeamMembers :: [DiscordTeamMember]
  , discordTeamName :: T.Text
  , discordTeamOwnerUserId :: UserId
  } deriving (Show, Read, Eq, Ord)

instance FromJSON DiscordTeam where
  parseJSON = withObject "DiscordTeam" $ \o ->
    DiscordTeam <$> o .:? "icon" -- ^ Hash of the image of the team's icon
                <*> o .:  "id"
                <*> o .:  "members"
                <*> o .:  "name"
                <*> o .:  "owner_user_id" -- ^ User ID of the current team owner

data DiscordTeamMember = DiscordTeamMember
  { discordTeamMemberState :: DiscordTeamMemberState
  , discordTeamMemberParentTeam :: DiscordTeamId
  , discordTeamMemberUserId :: UserId
  , discordTeamMemberOwnerRole :: T.Text
  } deriving (Show, Read, Eq, Ord)

instance FromJSON DiscordTeamMember where
  parseJSON = withObject "DiscordTeamMember" $ \o ->
    DiscordTeamMember <$> (parseState <$> (o .: "membership_state"))
                      <*> o .: "team_id"
                      <*> (o .: "user" >>= (.: "id"))
                      <*> o .: "role"
    where
      parseState :: Integer -> DiscordTeamMemberState
      parseState 1 = DiscordTeamMemberStateInvited
      parseState 2 = DiscordTeamMemberStateAccepted
      parseState _ = DiscordTeamMemberStateUnknown

data DiscordTeamMemberState = DiscordTeamMemberStateInvited
                            | DiscordTeamMemberStateAccepted
                            | DiscordTeamMemberStateUnknown
  deriving (Show, Read, Eq, Ord)



data DiscordInstallParams = DiscordInstallParams
    { discordInstallParamsScopes :: [T.Text]
    , discordInstallParamsPermissions :: T.Text
    } deriving (Show, Read, Eq, Ord)

instance FromJSON DiscordInstallParams where
  parseJSON = withObject "DiscordInstallParams" $ \o ->
    DiscordInstallParams <$> o .: "scopes"
                         <*> o .: "permissions"
