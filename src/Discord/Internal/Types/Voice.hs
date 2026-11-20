{-# LANGUAGE OverloadedStrings #-}

-- | Data structures pertaining to Discord voice states
module Discord.Internal.Types.Voice where

import Data.Aeson
import qualified Data.Text as T
import Data.Time (UTCTime)

import Discord.Internal.Types.Prelude
import Discord.Internal.Types.User (GuildMember)

-- | A user's voice connection status. Detailed information can be found at <https://discord.com/developers/docs/resources/voice#voice-state-object>.
data VoiceState = VoiceState
  { voiceStateGuildId :: Maybe GuildId -- ^ Guild the voice state is for, or 'Nothing' where Discord does not send it. The library adds it to each state in @GUILD_CREATE@
  , voiceStateChannelId :: Maybe ChannelId -- ^ Channel the user is connected to, or 'Nothing' if they have disconnected
  , voiceStateUserId :: UserId -- ^ User the voice state is for
  , voiceStateMember :: Maybe GuildMember -- ^ Guild member the voice state is for, or 'Nothing' where Discord does not send it
  , voiceStateSessionId :: T.Text -- ^ Session id of the voice state
  , voiceStateDeaf :: Bool -- ^ Whether the user is deafened by the server
  , voiceStateMute :: Bool -- ^ Whether the user is muted by the server
  , voiceStateSelfDeaf :: Bool -- ^ Whether the user is locally deafened
  , voiceStateSelfMute :: Bool -- ^ Whether the user is locally muted
  , voiceStateSelfStream :: Maybe Bool -- ^ Whether the user is streaming using \"Go Live\", or 'Nothing' where Discord sends @null@ or leaves it out
  , voiceStateSelfVideo :: Bool -- ^ Whether the user's camera is enabled
  , voiceStateSuppress :: Bool -- ^ Whether the user's permission to speak is denied
  , voiceStateRequestToSpeakTimestamp :: Maybe UTCTime -- ^ When the user requested to speak (in a stage channel)
  } deriving (Show, Read, Eq, Ord)

instance FromJSON VoiceState where
  parseJSON = withObject "VoiceState" $ \o ->
    VoiceState <$> o .:? "guild_id"
               <*> o .:? "channel_id"
               <*> o .:  "user_id"
               <*> o .:? "member"
               <*> o .:  "session_id"
               <*> o .:  "deaf"
               <*> o .:  "mute"
               <*> o .:  "self_deaf"
               <*> o .:  "self_mute"
               <*> o .:? "self_stream"
               <*> o .:  "self_video"
               <*> o .:  "suppress"
               <*> o .:? "request_to_speak_timestamp"
