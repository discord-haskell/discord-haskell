{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Data structures pertaining to Discord User
module Discord.Internal.Types.User where

import Data.Aeson
import Data.Text (Text)
import qualified Data.Text as T
import Discord.Internal.Types.Prelude

-- | Represents information about a user.
data User = User
  { userId       :: UserId       -- ^ The user's id.
  , userName     :: T.Text       -- ^ The user's username (not unique)
  , userDiscrim  :: T.Text       -- ^ The user's 4-digit discord-tag.
  , userAvatar   :: Maybe T.Text -- ^ The user's avatar hash.
  , userIsBot    :: Bool         -- ^ User is an OAuth2 application.
  , userIsWebhook:: Bool         -- ^ User is a webhook
  , userMfa      :: Maybe Bool   -- ^ User has two factor authentication enabled on the account.
  , userVerified :: Maybe Bool   -- ^ Whether the email has been verified.
  , userEmail    :: Maybe T.Text -- ^ The user's email.
  } deriving (Show, Eq, Ord)

instance FromJSON User where
  parseJSON = withObject "User" $ \o ->
    User <$> o .:  "id"
         <*> o .:  "username"
         <*> o .:  "discriminator"
         <*> o .:? "avatar"
         <*> o .:? "bot" .!= False
         <*> pure False -- webhook
         <*> o .:? "mfa_enabled"
         <*> o .:? "verified"
         <*> o .:? "email"

instance ToJSON User where
  toJSON User{..} = object [(name,value) | (name, Just value) <-
              [ ("id",            toJSON <$> pure userId)
              , ("username",      toJSON <$> pure userName)
              , ("discriminator", toJSON <$> pure userDiscrim)
              , ("avatar",        toJSON <$>      userAvatar)
              , ("bot",           toJSON <$> pure userIsBot)
              , ("webhook",       toJSON <$> pure userIsWebhook)
              , ("mfa_enabled",   toJSON <$>      userMfa)
              , ("verified",      toJSON <$>      userVerified)
              , ("email",         toJSON <$>      userEmail)
              ] ]

data Webhook = Webhook
  { webhookId :: WebhookId
  , webhookToken :: Text
  , webhookChannelId :: ChannelId
  } deriving (Show, Eq, Ord)

instance FromJSON Webhook where
  parseJSON = withObject "Webhook" $ \o ->
    Webhook <$> o .:  "id"
            <*> o .:  "token"
            <*> o .:  "channel_id"

data ConnectionObject = ConnectionObject
  { connectionObjectId :: Text
  , connectionObjectName :: Text
  , connectionObjectType :: Text
  , connectionObjectRevoked :: Bool
  , connectionObjectIntegrations :: [IntegrationId]
  , connectionObjectVerified :: Bool
  , connectionObjectFriendSyncOn :: Bool
  , connectionObjectShownInPresenceUpdates :: Bool
  , connectionObjectVisibleToOthers :: Bool
  } deriving (Show, Eq, Ord)

instance FromJSON ConnectionObject where
  parseJSON = withObject "ConnectionObject" $ \o -> do
    integrations <- o .: "integrations"
    ConnectionObject <$> o .: "id"
               <*> o .: "name"
               <*> o .: "type"
               <*> o .: "revoked"
               <*> sequence (map (.: "id") integrations)
               <*> o .: "verified"
               <*> o .: "friend_sync"
               <*> o .: "show_activity"
               <*> ( (==) (1::Int) <$> o .: "visibility")
