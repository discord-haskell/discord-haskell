{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Structures pertaining to Discord Scheduled Events
module Discord.Internal.Types.ScheduledEvents where

import           Data.Aeson                     ( (.=)
                                                , ToJSON(toJSON)
                                                , Value(Null, Number, Object)
                                                , object
                                                )
import           Data.HashMap.Strict            ( insert )
import           Data.Maybe                     ( isJust )
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

data ScheduledEvent
  = ScheduledEventStage
      { scheduledEventStageId :: ScheduledEventId
      , scheduledEventStageGuildId :: GuildId
      , scheduledEventStageChannelId :: ChannelId
      , scheduledEventStageCreatorId :: Maybe UserId
      , scheduledEventStageName :: T.Text
      , scheduledEventStageDescription :: Maybe T.Text
      , scheduledEventStageStartTime :: UTCTime
      , scheduledEventStageEndTime :: Maybe UTCTime
      , scheduledEventStagePrivacyLevel :: ScheduledEventPrivacyLevel
      , scheduledEventStageStatus :: ScheduledEventStatus
      , scheduledEventStageEntityId :: Maybe ScheduledEventEntityId
      , scheduledEventStageCreator :: Maybe User
      , scheduledEventStageUserCount :: Maybe Integer
        -- | FIXME: that's a compatible type, but idealy that's something we want a dedicated type for
      , scheduledEventStageImage :: Maybe T.Text
      }
  | ScheduledEventVoice
      { scheduledEventVoiceId :: ScheduledEventId
      , scheduledEventVoiceGuildId :: GuildId
      , scheduledEventVoiceChannelId :: ChannelId
      , scheduledEventVoiceCreatorId :: Maybe UserId
      , scheduledEventVoiceName :: T.Text
      , scheduledEventVoiceDescription :: Maybe T.Text
      , scheduledEventVoiceStartTime :: UTCTime
      , scheduledEventVoiceEndTime :: Maybe UTCTime
      , scheduledEventVoicePrivacyLevel :: ScheduledEventPrivacyLevel
      , scheduledEventVoiceStatus :: ScheduledEventStatus
      , scheduledEventVoiceEntityId :: Maybe ScheduledEventEntityId
      , scheduledEventVoiceCreator :: Maybe User
      , scheduledEventVoiceUserCount :: Maybe Integer
        -- | FIXME: that's a compatible type, but idealy that's something we want a dedicated type for
      , scheduledEventVoiceImage :: Maybe T.Text
      }
  | ScheduledEventExternal
      { scheduledEventExternalId :: ScheduledEventId
      , scheduledEventExternalGuildId :: GuildId
      , scheduledEventExternalLocation :: T.Text
      , scheduledEventExternalCreatorId :: Maybe UserId
      , scheduledEventExternalName :: T.Text
      , scheduledEventExternalDescription :: Maybe T.Text
      , scheduledEventExternalStartTime :: UTCTime
      , scheduledEventExternalEndTime :: UTCTime
      , scheduledEventExternalPrivacyLevel :: ScheduledEventPrivacyLevel
      , scheduledEventExternalStatus :: ScheduledEventStatus
      , scheduledEventExternalEntityId :: Maybe ScheduledEventEntityId
      , scheduledEventExternalCreator :: Maybe User
      , scheduledEventExternalUserCount :: Maybe Integer
        -- | FIXME: that's a compatible type, but idealy that's something we want a dedicated type for
      , scheduledEventExternalImage :: Maybe T.Text
      }

instance ToJSON ScheduledEvent where
  toJSON ScheduledEventExternal {..} =
    Object
      $ (if isJust scheduledEventExternalUserCount
          then insert "user_count" (toJSON scheduledEventExternalUserCount)
          else id
        )
      $ (if isJust scheduledEventExternalCreator
          then insert "creator" (toJSON scheduledEventExternalCreator)
          else id
        )
          base
   where
    Object base = object
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
  toJSON ScheduledEventStage {..} =
    Object
      $ (if isJust scheduledEventStageUserCount
          then insert "user_count" (toJSON scheduledEventStageUserCount)
          else id
        )
      $ (if isJust scheduledEventStageCreator
          then insert "creator" (toJSON scheduledEventStageCreator)
          else id
        )
          base
   where
    Object base = object
      [ "id" .= scheduledEventStageId
      , "guildId" .= scheduledEventStageGuildId
      , "channel_id" .= scheduledEventStageChannelId
      , "creator_id" .= scheduledEventStageCreatorId
      , "name" .= scheduledEventStageName
      , "description" .= scheduledEventStageDescription
      , "scheduled_start_time" .= scheduledEventStageStartTime
      , "scheduled_end_time" .= scheduledEventStageEndTime
      , "privacy_level" .= scheduledEventStagePrivacyLevel
      , "status" .= scheduledEventStageStatus
      , "entity_type" .= Number 1
      , "entity_id" .= scheduledEventStageEntityId
      , "entity_metadata" .= Null
      , "creator" .= scheduledEventStageCreator
      , "user_count" .= scheduledEventStageUserCount
      , "image" .= scheduledEventStageImage
      ]
  toJSON ScheduledEventVoice {..} =
    Object
      $ (if isJust scheduledEventVoiceUserCount
          then insert "user_count" (toJSON scheduledEventVoiceUserCount)
          else id
        )
      $ (if isJust scheduledEventVoiceCreator
          then insert "creator" (toJSON scheduledEventVoiceCreator)
          else id
        )
          base
   where
    Object base = object
      [ "id" .= scheduledEventVoiceId
      , "guildId" .= scheduledEventVoiceGuildId
      , "channel_id" .= scheduledEventVoiceChannelId
      , "creator_id" .= scheduledEventVoiceCreatorId
      , "name" .= scheduledEventVoiceName
      , "description" .= scheduledEventVoiceDescription
      , "scheduled_start_time" .= scheduledEventVoiceStartTime
      , "scheduled_end_time" .= scheduledEventVoiceEndTime
      , "privacy_level" .= scheduledEventVoicePrivacyLevel
      , "status" .= scheduledEventVoiceStatus
      , "entity_type" .= Number 2
      , "entity_id" .= scheduledEventVoiceEntityId
      , "entity_metadata" .= Null
      , "creator" .= scheduledEventVoiceCreator
      , "user_count" .= scheduledEventVoiceUserCount
      , "image" .= scheduledEventVoiceImage
      ]

data ScheduledEventPrivacyLevel = ScheduledEventPrivacyLevelGuildOnly

instance ToJSON ScheduledEventPrivacyLevel where
  toJSON ScheduledEventPrivacyLevelGuildOnly = Number 2

data ScheduledEventStatus
  = ScheduledEventStatusScheduled
  | ScheduledEventStatusActive
  | ScheduledEventStatusCompleted
  | ScheduledEventStatusCancelled

instance ToJSON ScheduledEventStatus where
  toJSON ScheduledEventStatusScheduled = Number 1
  toJSON ScheduledEventStatusActive    = Number 2
  toJSON ScheduledEventStatusCompleted = Number 3
  toJSON ScheduledEventStatusCancelled = Number 4
