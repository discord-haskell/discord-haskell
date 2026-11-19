{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Provides actions for Stage Instance API interactions
module Discord.Internal.Rest.StageInstance
  ( StageInstanceRequest(..)
  , CreateStageInstanceOpts(..)
  , ModifyStageInstanceOpts(..)
  ) where

import Data.Aeson
import Data.Default (Default(..))
import qualified Data.Text as T
import Network.HTTP.Req ((/:), (/~))
import qualified Network.HTTP.Req as R

import Discord.Internal.Rest.Prelude
import Discord.Internal.Types

instance Request (StageInstanceRequest a) where
  majorRoute = stageInstanceMajorRoute
  jsonRequest = stageInstanceJsonRequest

-- | Data constructor for requests. See <https://discord.com/developers/docs/resources/stage-instance API>
data StageInstanceRequest a where
  -- | Creates a 'StageInstance' in a stage channel. The current user must be a
  --   moderator of the channel, which takes the 'MANAGE_CHANNELS',
  --   'MUTE_MEMBERS' and 'MOVE_MEMBERS' permissions. Fires a Stage Instance
  --   Create 'Event'.
  CreateStageInstance :: CreateStageInstanceOpts -> StageInstanceRequest StageInstance
  -- | Returns the 'StageInstance' of a stage channel, if there is one.
  GetStageInstance :: ChannelId -> StageInstanceRequest StageInstance
  -- | Modifies the 'StageInstance' of a stage channel. The current user must be
  --   a moderator of the channel. Fires a Stage Instance Update 'Event'.
  ModifyStageInstance :: ChannelId -> ModifyStageInstanceOpts -> StageInstanceRequest StageInstance
  -- | Deletes the 'StageInstance' of a stage channel. The current user must be
  --   a moderator of the channel. Fires a Stage Instance Delete 'Event'.
  DeleteStageInstance :: ChannelId -> StageInstanceRequest ()

-- | Options for `CreateStageInstance`
data CreateStageInstanceOpts = CreateStageInstanceOpts
  { createStageInstanceOptsChannelId :: ChannelId -- ^ The stage channel
  , createStageInstanceOptsTopic :: T.Text -- ^ Topic of the stage instance, from 1 to 120 characters
  , createStageInstanceOptsPrivacyLevel :: Maybe StageInstancePrivacyLevel -- ^ Who can see the stage instance. Discord defaults it to guild only
  , createStageInstanceOptsSendStartNotification :: Maybe Bool -- ^ Whether to notify @\@everyone@ that the stage instance started. Requires the 'MENTION_EVERYONE' permission
  , createStageInstanceOptsGuildScheduledEventId :: Maybe ScheduledEventId -- ^ A scheduled event to link to the stage instance
  } deriving (Show, Read, Eq, Ord)

instance ToJSON CreateStageInstanceOpts where
  toJSON CreateStageInstanceOpts{..} = objectFromMaybes
                       [ "channel_id" .== createStageInstanceOptsChannelId
                       , "topic" .== createStageInstanceOptsTopic
                       , "privacy_level" .=? createStageInstanceOptsPrivacyLevel
                       , "send_start_notification" .=? createStageInstanceOptsSendStartNotification
                       , "guild_scheduled_event_id" .=? createStageInstanceOptsGuildScheduledEventId ]

-- | Options for `ModifyStageInstance`
data ModifyStageInstanceOpts = ModifyStageInstanceOpts
  { modifyStageInstanceOptsTopic :: Maybe T.Text -- ^ Topic of the stage instance, from 1 to 120 characters
  , modifyStageInstanceOptsPrivacyLevel :: Maybe StageInstancePrivacyLevel -- ^ Who can see the stage instance
  } deriving (Show, Read, Eq, Ord)

instance Default ModifyStageInstanceOpts where
  def = ModifyStageInstanceOpts Nothing Nothing

instance ToJSON ModifyStageInstanceOpts where
  toJSON ModifyStageInstanceOpts{..} = objectFromMaybes
                       [ "topic" .=? modifyStageInstanceOptsTopic
                       , "privacy_level" .=? modifyStageInstanceOptsPrivacyLevel ]

stageInstanceMajorRoute :: StageInstanceRequest a -> String
stageInstanceMajorRoute c = case c of
  (CreateStageInstance o) -> "stage_instance " <> show (createStageInstanceOptsChannelId o)
  (GetStageInstance ch) -> "stage_instance " <> show ch
  (ModifyStageInstance ch _) -> "stage_instance " <> show ch
  (DeleteStageInstance ch) -> "stage_instance " <> show ch

stageInstances :: R.Url 'R.Https
stageInstances = baseUrl /: "stage-instances"

stageInstanceJsonRequest :: StageInstanceRequest r -> JsonRequest
stageInstanceJsonRequest c = case c of
  (CreateStageInstance o) -> Post stageInstances (pure $ R.ReqBodyJson $ toJSON o) mempty
  (GetStageInstance ch) -> Get (stageInstances /~ ch) mempty
  (ModifyStageInstance ch o) -> Patch (stageInstances /~ ch) (pure $ R.ReqBodyJson $ toJSON o) mempty
  (DeleteStageInstance ch) -> Delete (stageInstances /~ ch) mempty
