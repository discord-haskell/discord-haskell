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
  -- , fullApplicationBot :: User -- partial user, test what it has?
  , fullApplicationTermsOfServiceUrl :: Maybe T.Text
  , fullApplicationPrivacyPolicyUrl :: Maybe T.Text
  -- , fullApplicationOwner :: User -- ^ partial user, test what it has?
  , fullApplicationVerifyKey :: T.Text -- ^ Hex encoded key for verification in interactions and the GameSDK's GetTicket
  -- , fullApplicationTeam :: Team -- ^
  , fullApplicationGuildID :: Maybe GuildId -- ^ Guild ID associated with the App. 
  -- fullApplicationGuild :: PartialGuild
  -- , fullApplicationPrimaryGameSKU
  -- , fullApplicationSlug 
  , fullApplicationCoverImage :: Maybe T.Text -- ^ App's default rich presence invite cover image hash
  , fullApplicationFlags :: Maybe Integer -- ^ App's public flags
  , fullApplicationApproximateGuildCount :: Maybe Integer -- ^ Approximate count of guilds the app has been added to
  , fullApplicationRedirectURIs :: Maybe [T.Text] -- ^ Array of redirect URIs for the app
  , fullApplicationInteractionEndpointURL :: Maybe T.Text -- ^ Interactions endpoint URL for the app
  , fullApplicationRoleConnectionVerificationURL :: Maybe T.Text -- ^ Role connection verification URL for the app
  , fullApplicationTags :: Maybe [T.Text] -- ^ List of tags describing the content and functionality of the app. Max of 5 tags.
  -- , fullApplicationInstallParams
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
                    -- <*> o .:? "bot"
                    <*> o .:? "terms_of_service_url"
                    <*> o .:? "privacy_policy_url"
                    -- <*> o .:? "owner"
                    <*> o .: "verify_key"
                    -- <*> o .:? "team"
                    <*> o .:? "guild_id"
                    -- <*> o .:? "guild"
                    -- <*> o .:? "primary_sku_id"
                    -- <*> o .:? "slug"
                    <*> o .:? "cover_image"
                    <*> o .:? "flags"
                    <*> o .:? "approximate_guild_count"
                    <*> o .:? "redirect_uris"
                    <*> o .:? "interactions_endpoint_url"
                    <*> o .:? "role_connections_verification_url"
                    <*> o .:? "tags"
                    -- <*> o .:? "install_params"
                    <*> o .:? "custom_install_url"


