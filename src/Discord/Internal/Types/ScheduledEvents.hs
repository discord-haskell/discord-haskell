{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveDataTypeable #-}

-- | Structures pertaining to Discord Scheduled Events
module Discord.Internal.Types.ScheduledEvents where

import           Data.Aeson                     ( (.:)
                                                , (.:?)
                                                , (.=)
                                                , FromJSON(parseJSON)
                                                , ToJSON(toJSON)
                                                , Value(Number)
                                                , object
                                                , withObject
                                                )
import           Data.Aeson.Types               ( Parser )
import           Data.Data                      ( Data )
import qualified Data.Text                     as T
import           Data.Time                      ( UTCTime )
import           Discord.Internal.Types.Prelude ( ChannelId
                                                , GuildId
                                                , InternalDiscordEnum
                                                  ( discordTypeParseJSON
                                                  , discordTypeStartValue
                                                  , fromDiscordType
                                                  )
                                                , Snowflake
                                                , UserId
                                                , toMaybeJSON
                                                )
import           Discord.Internal.Types.User    ( User )


type ScheduledEventId = Snowflake

type ScheduledEventEntityId = Snowflake

-- | The ScheduledEvent data structure
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
  toJSON ScheduledEventStage {..} = object
    [ (name, value)
    | (name, Just value) <-
      [ ("id"                  , toMaybeJSON scheduledEventStageId)
      , ("guild_id"            , toMaybeJSON scheduledEventStageGuildId)
      , ("channel_id"          , toMaybeJSON scheduledEventStageChannelId)
      , ("creator_id"          , toJSON <$> scheduledEventStageCreatorId)
      , ("name"                , toMaybeJSON scheduledEventStageName)
      , ("description"         , toJSON <$> scheduledEventStageDescription)
      , ("scheduled_start_time", toMaybeJSON scheduledEventStageStartTime)
      , ("scheduled_end_time"  , toJSON <$> scheduledEventStageEndTime)
      , ("privacy_level"       , toMaybeJSON scheduledEventStagePrivacyLevel)
      , ("entity_type"         , Just $ Number 1)
      , ("entity_id"           , toJSON <$> scheduledEventStageEntityId)
      , ("creator"             , toJSON <$> scheduledEventStageCreator)
      , ("user_count"          , toJSON <$> scheduledEventStageUserCount)
      , ("image"               , toJSON <$> scheduledEventStageImage)
      ]
    ]
  toJSON ScheduledEventVoice {..} = object
    [ (name, value)
    | (name, Just value) <-
      [ ("id"                  , toMaybeJSON scheduledEventVoiceId)
      , ("guild_id"            , toMaybeJSON scheduledEventVoiceGuildId)
      , ("channel_id"          , toMaybeJSON scheduledEventVoiceChannelId)
      , ("creator_id"          , toJSON <$> scheduledEventVoiceCreatorId)
      , ("name"                , toMaybeJSON scheduledEventVoiceName)
      , ("description"         , toJSON <$> scheduledEventVoiceDescription)
      , ("scheduled_start_time", toMaybeJSON scheduledEventVoiceStartTime)
      , ("scheduled_end_time"  , toJSON <$> scheduledEventVoiceEndTime)
      , ("privacy_level"       , toMaybeJSON scheduledEventVoicePrivacyLevel)
      , ("entity_type"         , Just $ Number 2)
      , ("entity_id"           , toJSON <$> scheduledEventVoiceEntityId)
      , ("creator"             , toJSON <$> scheduledEventVoiceCreator)
      , ("user_count"          , toJSON <$> scheduledEventVoiceUserCount)
      , ("image"               , toJSON <$> scheduledEventVoiceImage)
      ]
    ]
  toJSON ScheduledEventExternal {..} = object
    [ (name, value)
    | (name, Just value) <-
      [ ("id"                  , toMaybeJSON scheduledEventExternalId)
      , ("guild_id"            , toMaybeJSON scheduledEventExternalGuildId)
      , ("creator_id"          , toJSON <$> scheduledEventExternalCreatorId)
      , ("name"                , toMaybeJSON scheduledEventExternalName)
      , ("description"         , toJSON <$> scheduledEventExternalDescription)
      , ("scheduled_start_time", toMaybeJSON scheduledEventExternalStartTime)
      , ("scheduled_end_time"  , toMaybeJSON scheduledEventExternalEndTime)
      , ("privacy_level", toMaybeJSON scheduledEventExternalPrivacyLevel)
      , ("entity_type"         , Just $ Number 3)
      , ("entity_id"           , toJSON <$> scheduledEventExternalEntityId)
      , ("creator"             , toJSON <$> scheduledEventExternalCreator)
      , ("user_count"          , toJSON <$> scheduledEventExternalUserCount)
      , ("image"               , toJSON <$> scheduledEventExternalImage)
      , ( "entity_metadata"
        , Just $ object ["location" .= toJSON scheduledEventExternalLocation]
        )
      ]
    ]


instance FromJSON ScheduledEvent where
  parseJSON = withObject
    "ScheduledEvent"
    (\v -> do
      setype <- v .: "entity_type" :: Parser Int
      seid   <- v .: "id"
      segid  <- v .: "guild_id"
      secrid <- v .:? "creator_id"
      sename <- v .: "name"
      sedesc <- v .:? "description"
      sest   <- v .: "scheduled_start_time"
      sepl   <- v .: "privacy_level" :: Parser ScheduledEventPrivacyLevel
      sestat <- v .: "status" :: Parser ScheduledEventStatus
      seeid  <- v .:? "entity_id"
      secrea <- v .:? "creator"
      seuc   <- v .:? "user_count"
      seim   <- v .:? "image"

      case setype of
        1 -> do
          sechid <- v .: "channelId"
          seet   <- v .:? "scheduled_end_time"
          return $ ScheduledEventStage seid
                                       segid
                                       sechid
                                       secrid
                                       sename
                                       sedesc
                                       sest
                                       seet
                                       sepl
                                       sestat
                                       seeid
                                       secrea
                                       seuc
                                       seim
        2 -> do
          sechid <- v .: "channelId"
          seet   <- v .:? "scheduled_end_time"
          return $ ScheduledEventVoice seid
                                       segid
                                       sechid
                                       secrid
                                       sename
                                       sedesc
                                       sest
                                       seet
                                       sepl
                                       sestat
                                       seeid
                                       secrea
                                       seuc
                                       seim
        3 -> do
          semeta <- v .: "entity_metadata"
          seloc  <- withObject "entity_metadata" (.: "location") semeta
          seet   <- v .: "scheduled_end_time"
          return $ ScheduledEventExternal seid
                                          segid
                                          seloc
                                          secrid
                                          sename
                                          sedesc
                                          sest
                                          seet
                                          sepl
                                          sestat
                                          seeid
                                          secrea
                                          seuc
                                          seim
        _ -> error "unreachable"
    )

-- | The privacy level of a scheduled event
data ScheduledEventPrivacyLevel = ScheduledEventPrivacyLevelGuildOnly
  deriving (Show, Read, Eq, Ord, Data)

instance InternalDiscordEnum ScheduledEventPrivacyLevel where
  discordTypeStartValue = ScheduledEventPrivacyLevelGuildOnly
  fromDiscordType ScheduledEventPrivacyLevelGuildOnly = 2

instance ToJSON ScheduledEventPrivacyLevel where
  toJSON = toJSON . fromDiscordType

instance FromJSON ScheduledEventPrivacyLevel where
  parseJSON = discordTypeParseJSON "ScheduledEventPrivacyLevel"

-- | The Status of a Scheduled Event
data ScheduledEventStatus
  = ScheduledEventStatusScheduled
  | ScheduledEventStatusActive
  | ScheduledEventStatusCompleted
  | ScheduledEventStatusCancelled
  deriving (Show, Read, Eq, Ord, Data)

instance InternalDiscordEnum ScheduledEventStatus where
  discordTypeStartValue = ScheduledEventStatusScheduled
  fromDiscordType ScheduledEventStatusScheduled = 1
  fromDiscordType ScheduledEventStatusActive    = 2
  fromDiscordType ScheduledEventStatusCompleted = 3
  fromDiscordType ScheduledEventStatusCancelled = 4

instance ToJSON ScheduledEventStatus where
  toJSON = toJSON . fromDiscordType

instance FromJSON ScheduledEventStatus where
  parseJSON = discordTypeParseJSON "ScheduledEventStatus"
