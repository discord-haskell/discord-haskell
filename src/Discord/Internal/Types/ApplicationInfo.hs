{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Data structures pertaining to Discord User
module Discord.Internal.Types.ApplicationInfo where

import Data.Aeson
import qualified Data.Text as T
import Discord.Internal.Types.Prelude

-- | Structure containing partial information about an Application
data FullApplication = FullApplication
  { fullApplicationID :: ApplicationId
  , fullApplicationName :: T.Text
  , fullApplicationIcon :: Maybe T.Text -- ^ Icon hash
  , fullApplicationDescription :: Maybe T.Text
  , fullApplicationRPCOrigins :: [T.Text] -- ^ List of RPC origin URLs, if RPC is enabled
  , fullApplicationBotPublic :: Bool -- ^ When false, only the app owner can add the app to guilds
  , fullApplicationRequiresCodeGrant Bool -- ^ When true, the app's bot will only join upon completion of the full OAuth2 code grant flow
  -- , fullApplicationBot :: User -- partial user, test what it has?
  , fullApplicationTermsOfServiceUrl :: T.Text
  , fullApplicationPrivacyPolicyUrl :: T.Text
  -- , fullApplicationOwner :: User -- ^ partial user, test what it has?
  , fullApplicationVerifyKey :: T.Text -- ^ Hex encoded key for verification in interactions and the GameSDK's GetTicket
  -- , fullApplicationTeam :: Team -- ^ 

instance FromJSON FullApplication where
  parseJSON = withObject "FullApplication" $ \o ->
    FullApplication <$> o .: "id"
                    <*> o .: "name"
                    <*> o .: "flags"

