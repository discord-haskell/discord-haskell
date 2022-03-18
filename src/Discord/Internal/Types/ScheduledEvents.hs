{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Structures pertaining to Discord Scheduled Events
module Discord.Internal.Types.ScheduledEvents where

import           Data.Aeson                     ( (.=)
                                                , ToJSON(toJSON)
                                                , Value(Null, Number, Object)
                                                , object
                                                )
import qualified Data.Text                     as T
import           Data.Time                      ( UTCTime )
import           Discord.Internal.Types.Prelude ( ChannelId
                                                , GuildId
                                                , Snowflake
                                                , UserId
                                                )
import           Discord.Internal.Types.User    ( User )

type ScheduledEventId = Snowflake
type ScheduledEventEntityId = Snowflake

data ScheduledEvent =
    ScheduledEventStage { scheduledEventStageId           :: ScheduledEventId
                        , scheduledEventStageGuildId      :: GuildId
                        , scheduledEventStageChannelId    :: ChannelId
                        , scheduledEventStageCreatorId    :: Maybe UserId
                        , scheduledEventStageName         :: T.Text
                        , scheduledEventStageDescription  :: Maybe T.Text
                        , scheduledEventStageStartTime    :: UTCTime
                        , scheduledEventStageEndTime      :: Maybe UTCTime
                        , scheduledEventStagePrivacyLevel :: ScheduledEventPrivacyLevel
                        , scheduledEventStageStatus       :: ScheduledEventStatus
                        , scheduledEventStageEntityId     :: Maybe ScheduledEventEntityId
                        , scheduledEventStageCreator      :: Maybe User
                        , scheduledEventStageUserCount    :: Integer
                        , scheduledEventStageImage        :: Maybe T.Text
                          -- ^ FIXME: that's a compatible type, but idealy that's something we want a dedicated type for
                        }
    | ScheduledEventVoice { scheduledEventVoiceId           :: ScheduledEventId
                          , scheduledEventVoiceGuildId      :: GuildId
                          , scheduledEventVoiceChannelId    :: ChannelId
                          , scheduledEventVoiceCreatorId    :: Maybe UserId
                          , scheduledEventVoiceName         :: T.Text
                          , scheduledEventVoiceDescription  :: Maybe T.Text
                          , scheduledEventVoiceStartTime    :: UTCTime
                          , scheduledEventVoiceEndTime      :: Maybe UTCTime
                          , scheduledEventVoicePrivacyLevel :: ScheduledEventPrivacyLevel
                          , scheduledEventVoiceStatus       :: ScheduledEventStatus
                          , scheduledEventVoiceEntityId     :: Maybe ScheduledEventEntityId
                          , scheduledEventVoiceCreator      :: Maybe User
                          , scheduledEventVoiceUserCount    :: Integer
                          , scheduledEventVoiceImage        :: Maybe T.Text
                            -- ^ FIXME: that's a compatible type, but idealy that's something we want a dedicated type for
                          }
    | ScheduledEventExternal { scheduledEventExternalId           :: ScheduledEventId
                             , scheduledEventExternalGuildId      :: GuildId
                             , scheduledEventExternalLocation     :: T.Text
                             , scheduledEventExternalCreatorId    :: Maybe UserId
                             , scheduledEventExternalName         :: T.Text
                             , scheduledEventExternalDescription  :: Maybe T.Text
                             , scheduledEventExternalStartTime    :: UTCTime
                             , scheduledEventExternalEndTime      :: UTCTime
                             , scheduledEventExternalPrivacyLevel :: ScheduledEventPrivacyLevel
                             , scheduledEventExternalStatus       :: ScheduledEventStatus
                             , scheduledEventExternalEntityId     :: Maybe ScheduledEventEntityId
                             , scheduledEventExternalCreator      :: Maybe User
                             , scheduledEventExternalUserCount    :: Integer
                             , scheduledEventExternalImage        :: Maybe T.Text
                               -- ^ FIXME: that's a compatible type, but idealy that's something we want a dedicated type for
                             }

instance ToJSON ScheduledEvent where
    toJSON ScheduledEventExternal {..} = object
        [ "id" .= scheduledEventExternalId
        , "guild_id" .= scheduledEventExternalGuildId
        , "channel_id" .= Null
        , "name" .= scheduledEventExternalName
        , "creator_id" .= scheduledEventExternalCreatorId
        , "description" .= scheduledEventExternalDescription
        , "scheduled_start_time" .= scheduledEventExternalStartTime
        , "scheduled_end_time" .= scheduledEventExternalEndTime
        , "privacy_level" .= scheduledEventExternalPrivacyLevel
        , "status" .= scheduledEventExternalStatus
        , "entity_type" .= Number 3
        , "entity_id" .= scheduledEventExternalEntityId
        , "entity_metadata"
            .= object ["location" .= scheduledEventExternalLocation]
        , "creator" .= scheduledEventExternalCreator
        , "user_count" .= scheduledEventExternalUserCount
        , "image" .= scheduledEventExternalImage
        ]
    toJSON ScheduledEventStage {..} = object []
    toJSON ScheduledEventVoice {..} = object []

data ScheduledEventPrivacyLevel = ScheduledEventPrivacyLevelGuildOnly

instance ToJSON ScheduledEventPrivacyLevel where
    toJSON ScheduledEventPrivacyLevelGuildOnly = Number 2

data ScheduledEventStatus =
  ScheduledEventStatusScheduled
  | ScheduledEventStatusActive
  | ScheduledEventStatusCompleted
  | ScheduledEventStatusCancelled

instance ToJSON ScheduledEventStatus where
    toJSON ScheduledEventStatusScheduled = Number 1
    toJSON ScheduledEventStatusActive    = Number 2
    toJSON ScheduledEventStatusCompleted = Number 3
    toJSON ScheduledEventStatusCancelled = Number 4
