{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Provides actions for Voice API interactions
module Discord.Internal.Rest.Voice
  ( VoiceRequest(..)
  , ModifyCurrentUserVoiceStateOpts(..)
  , ModifyUserVoiceStateOpts(..)
  ) where


import Data.Default (Default(..))
import Network.HTTP.Req ((/:), (/~))
import qualified Network.HTTP.Req as R

import Discord.Internal.Rest.Prelude
import Discord.Internal.Types

instance Request (VoiceRequest a) where
  majorRoute = voiceMajorRoute
  jsonRequest = voiceJsonRequest

-- | Data constructor for requests
data VoiceRequest a where
  -- | List all available 'VoiceRegion's.
  ListVoiceRegions :: VoiceRequest [VoiceRegion]
  -- | Returns the current user's 'VoiceState' in the guild.
  GetCurrentUserVoiceState :: GuildId -> VoiceRequest VoiceState
  -- | Returns the 'VoiceState' of a user in the guild. If the user is in a
  --   voice channel, the current user needs permission to connect to it.
  GetUserVoiceState :: GuildId -> UserId -> VoiceRequest VoiceState
  -- | Updates the current user's voice state in a stage channel. Fires a Voice
  --   State Update 'Event'.
  ModifyCurrentUserVoiceState :: GuildId -> ModifyCurrentUserVoiceStateOpts -> VoiceRequest ()
  -- | Updates another user's voice state in a stage channel. Requires the
  --   'MUTE_MEMBERS' permission. Fires a Voice State Update 'Event'.
  ModifyUserVoiceState :: GuildId -> UserId -> ModifyUserVoiceStateOpts -> VoiceRequest ()

-- | Options for `ModifyCurrentUserVoiceState`
data ModifyCurrentUserVoiceStateOpts = ModifyCurrentUserVoiceStateOpts
  { modifyCurrentUserVoiceStateOptsChannelId :: Maybe ChannelId -- ^ The stage channel the user is in, which they must already have joined
  , modifyCurrentUserVoiceStateOptsSuppress :: Maybe Bool -- ^ Whether the user is suppressed. Unsuppressing requires the 'MUTE_MEMBERS' permission
  , modifyCurrentUserVoiceStateOptsRequestToSpeakTimestamp :: Maybe UTCTime -- ^ When the user requests to speak, which can be now or in the future
  } deriving (Show, Read, Eq, Ord)

instance Default ModifyCurrentUserVoiceStateOpts where
  def = ModifyCurrentUserVoiceStateOpts Nothing Nothing Nothing

instance ToJSON ModifyCurrentUserVoiceStateOpts where
  toJSON ModifyCurrentUserVoiceStateOpts{..} = objectFromMaybes
                       [ "channel_id" .=? modifyCurrentUserVoiceStateOptsChannelId
                       , "suppress" .=? modifyCurrentUserVoiceStateOptsSuppress
                       , "request_to_speak_timestamp" .=? modifyCurrentUserVoiceStateOptsRequestToSpeakTimestamp ]

-- | Options for `ModifyUserVoiceState`
data ModifyUserVoiceStateOpts = ModifyUserVoiceStateOpts
  { modifyUserVoiceStateOptsChannelId :: Maybe ChannelId -- ^ The stage channel the user is in, which they must already have joined
  , modifyUserVoiceStateOptsSuppress :: Maybe Bool -- ^ Whether the user is suppressed
  } deriving (Show, Read, Eq, Ord)

instance Default ModifyUserVoiceStateOpts where
  def = ModifyUserVoiceStateOpts Nothing Nothing

instance ToJSON ModifyUserVoiceStateOpts where
  toJSON ModifyUserVoiceStateOpts{..} = objectFromMaybes
                       [ "channel_id" .=? modifyUserVoiceStateOptsChannelId
                       , "suppress" .=? modifyUserVoiceStateOptsSuppress ]

voiceMajorRoute :: VoiceRequest a -> String
voiceMajorRoute c = case c of
  (ListVoiceRegions) -> "whatever "
  (GetCurrentUserVoiceState g) -> "voice " <> show g
  (GetUserVoiceState g _) -> "voice " <> show g
  (ModifyCurrentUserVoiceState g _) -> "voice " <> show g
  (ModifyUserVoiceState g _ _) -> "voice " <> show g

voices :: R.Url 'R.Https
voices = baseUrl /: "voice"

voiceJsonRequest :: VoiceRequest r -> JsonRequest
voiceJsonRequest c = case c of
  (ListVoiceRegions) -> Get (voices /: "regions") mempty
  (GetCurrentUserVoiceState g) -> Get (voiceStates g /: "@me") mempty
  (GetUserVoiceState g u) -> Get (voiceStates g /~ u) mempty
  (ModifyCurrentUserVoiceState g o) ->
    Patch (voiceStates g /: "@me") (pure $ R.ReqBodyJson $ toJSON o) mempty
  (ModifyUserVoiceState g u o) ->
    Patch (voiceStates g /~ u) (pure $ R.ReqBodyJson $ toJSON o) mempty
  where
    voiceStates g = baseUrl /: "guilds" /~ g /: "voice-states"
