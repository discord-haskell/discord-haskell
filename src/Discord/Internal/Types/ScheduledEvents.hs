{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveDataTypeable #-}

-- | Structures pertaining to Discord Scheduled Events
module Discord.Internal.Types.ScheduledEvents where

import           Data.Aeson                     ( (.:)
                                                , (.:!)
                                                , (.:?)
                                                , (.=)
                                                , FromJSON(parseJSON)
                                                , ToJSON(toJSON)
                                                , Value(Null, Number, String)
                                                , object
                                                , withObject
                                                , withText
                                                )
import           Data.Aeson.Types               ( Parser )
import qualified Data.ByteString               as B
import           Data.Data                      ( Data )
import           Data.Default                   ( Default(def) )
import qualified Data.Text                     as T
import qualified Data.Text.Encoding            as T
import           Data.Time                      ( UTCTime )
import           Discord.Internal.Types.Prelude ( ChannelId
                                                , GuildId
                                                , InternalDiscordEnum
                                                  ( discordTypeParseJSON
                                                  , discordTypeStartValue
                                                  , fromDiscordType
                                                  )
                                                , ScheduledEventEntityId
                                                , ScheduledEventId
                                                , UserId
                                                , (.==)
                                                , (.=?)
                                                , objectFromMaybes
                                                )
import           Discord.Internal.Types.User    ( GuildMember
                                                , User
                                                )



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
      , scheduledEventStageImage :: Maybe ScheduledEventImageHash
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
      , scheduledEventVoiceImage :: Maybe ScheduledEventImageHash
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
      , scheduledEventExternalImage :: Maybe ScheduledEventImageHash
      }
  deriving (Show, Eq, Read)

instance ToJSON ScheduledEvent where
  toJSON ScheduledEventStage {..} = objectFromMaybes
      [ "id"                   .== scheduledEventStageId
      , "guild_id"             .== scheduledEventStageGuildId
      , "channel_id"           .== scheduledEventStageChannelId
      , "creator_id"           .=? scheduledEventStageCreatorId
      , "name"                 .== scheduledEventStageName
      , "description"          .=? scheduledEventStageDescription
      , "scheduled_start_time" .== scheduledEventStageStartTime
      , "scheduled_end_time"   .=? scheduledEventStageEndTime
      , "privacy_level"        .== scheduledEventStagePrivacyLevel
      , "entity_type"          .== Number 1
      , "entity_id"            .=? scheduledEventStageEntityId
      , "creator"              .=? scheduledEventStageCreator
      , "user_count"           .=? scheduledEventStageUserCount
      , "image"                .=? scheduledEventStageImage
      ]
  toJSON ScheduledEventVoice {..} = objectFromMaybes
      [ "id"                   .== scheduledEventVoiceId
      , "guild_id"             .== scheduledEventVoiceGuildId
      , "channel_id"           .== scheduledEventVoiceChannelId
      , "creator_id"           .=? scheduledEventVoiceCreatorId
      , "name"                 .== scheduledEventVoiceName
      , "description"          .=? scheduledEventVoiceDescription
      , "scheduled_start_time" .== scheduledEventVoiceStartTime
      , "scheduled_end_time"   .=? scheduledEventVoiceEndTime
      , "privacy_level"        .== scheduledEventVoicePrivacyLevel
      , "entity_type"          .== Number 2
      , "entity_id"            .=? scheduledEventVoiceEntityId
      , "creator"              .=? scheduledEventVoiceCreator
      , "user_count"           .=? scheduledEventVoiceUserCount
      , "image"                .=? scheduledEventVoiceImage
      ]
  toJSON ScheduledEventExternal {..} = objectFromMaybes
      [ "id"                   .== scheduledEventExternalId
      , "guild_id"             .== scheduledEventExternalGuildId
      , "creator_id"           .=? scheduledEventExternalCreatorId
      , "name"                 .== scheduledEventExternalName
      , "description"          .=? scheduledEventExternalDescription
      , "scheduled_start_time" .== scheduledEventExternalStartTime
      , "scheduled_end_time"   .== scheduledEventExternalEndTime
      , "privacy_level"        .== scheduledEventExternalPrivacyLevel
      , "entity_type"          .== Number 3
      , "entity_id"            .=? scheduledEventExternalEntityId
      , "creator"              .=? scheduledEventExternalCreator
      , "user_count"           .=? scheduledEventExternalUserCount
      , "image"                .=? scheduledEventExternalImage
      , "entity_metadata"
         .== object ["location" .= toJSON scheduledEventExternalLocation]
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

-- | The hash of the cover image of a ScheduledEvent
type ScheduledEventImageHash = T.Text

-- | The type of images that can be uploaded
data CreateScheduledEventImageUploadType
  = CreateScheduledEventImageUploadTypeJPG
  | CreateScheduledEventImageUploadTypePNG
  | CreateScheduledEventImageUploadTypeGIF
  deriving (Show, Read, Eq, Ord)

-- | The required information to add a cover image to a Scheduled Event
data CreateScheduledEventImage
  = CreateScheduledEventImageURL T.Text
  | CreateScheduledEventImageUpload CreateScheduledEventImageUploadType B.ByteString
  deriving (Show, Read, Eq, Ord)

instance ToJSON CreateScheduledEventImage where
  toJSON (CreateScheduledEventImageURL u) = String u
  toJSON (CreateScheduledEventImageUpload typ bs) =
    String
      $  "data:"
      <> (case typ of
           CreateScheduledEventImageUploadTypeJPG -> "image/jpeg"
           CreateScheduledEventImageUploadTypePNG -> "image/png"
           CreateScheduledEventImageUploadTypeGIF -> "image/gif"
         )
      <> ";base64,"
      <> T.decodeUtf8 bs

instance FromJSON CreateScheduledEventImage where
  parseJSON =
    withText "CreateScheduledEventImage" (return . CreateScheduledEventImageURL)

-- | Data required to create a Scheduled Event
data CreateScheduledEventData
  = CreateScheduledEventDataStage
      { createScheduleEventDataStageChannelId :: ChannelId
      , createScheduleEventDataStageName :: T.Text
      , createScheduleEventDataStagePrivacyLevel :: ScheduledEventPrivacyLevel
      , createScheduleEventDataStageStartTime :: UTCTime
      , createScheduleEventDataStageEndTime :: Maybe UTCTime
      , createScheduleEventDataStageDescription :: Maybe T.Text
      , createScheduleEventDataStageImage :: Maybe CreateScheduledEventImage
      }
  | CreateScheduledEventDataVoice
      { createScheduleEventDataVoiceChannelId :: ChannelId
      , createScheduleEventDataVoiceName :: T.Text
      , createScheduleEventDataVoicePrivacyLevel :: ScheduledEventPrivacyLevel
      , createScheduleEventDataVoiceStartTime :: UTCTime
      , createScheduleEventDataVoiceEndTime :: Maybe UTCTime
      , createScheduleEventDataVoiceDescription :: Maybe T.Text
      , createScheduleEventDataVoiceImage :: Maybe CreateScheduledEventImage
      }
  | CreateScheduledEventDataExternal
      { createScheduleEventDataExternalLocation :: T.Text
      , createScheduleEventDataExternalName :: T.Text
      , createScheduleEventDataExternalPrivacyLevel :: ScheduledEventPrivacyLevel
      , createScheduleEventDataExternalStartTime :: UTCTime
      , createScheduleEventDataExternalEndTime :: UTCTime
      , createScheduleEventDataExternalDescription :: Maybe T.Text
      , createScheduleEventDataExternalImage :: Maybe CreateScheduledEventImage
      }

instance ToJSON CreateScheduledEventData where
  toJSON CreateScheduledEventDataStage {..} = objectFromMaybes
      [ "channel_id"           .== createScheduleEventDataStageChannelId
      , "name"                 .== createScheduleEventDataStageName
      , "privacy_level"        .== createScheduleEventDataStagePrivacyLevel
      , "scheduled_start_time" .== createScheduleEventDataStageStartTime
      , "scheduled_end_time"   .=? createScheduleEventDataStageEndTime
      , "description"          .=? createScheduleEventDataStageDescription
      , "entity_type"          .== Number 1
      , "image"                .=? createScheduleEventDataStageImage
      ]
  toJSON CreateScheduledEventDataVoice {..} = objectFromMaybes
      [ "channel_id"           .== createScheduleEventDataVoiceChannelId
      , "name"                 .== createScheduleEventDataVoiceName
      , "privacy_level"        .== createScheduleEventDataVoicePrivacyLevel
      , "scheduled_start_time" .== createScheduleEventDataVoiceStartTime
      , "scheduled_end_time"   .=? createScheduleEventDataVoiceEndTime
      , "description"          .=? createScheduleEventDataVoiceDescription
      , "entity_type"          .== Number 2
      , "image"                .=? createScheduleEventDataVoiceImage
      ]
  toJSON CreateScheduledEventDataExternal {..} = objectFromMaybes
      [ "entity_metadata"
         .== object ["location" .= createScheduleEventDataExternalLocation]
      , "name"                 .== createScheduleEventDataExternalName
      , "privacy_level"        .== createScheduleEventDataExternalPrivacyLevel
      , "scheduled_start_time" .== createScheduleEventDataExternalStartTime
      , "scheduled_end_time"   .== createScheduleEventDataExternalEndTime
      , "description"          .=? createScheduleEventDataExternalDescription
      , "entity_type"          .== Number 2
      , "image"                .=? createScheduleEventDataExternalImage
      ]

instance FromJSON CreateScheduledEventData where
  parseJSON = withObject
    "CreateScheduledEventData"
    (\v -> do
      t       <- v .: "entity_type" :: Parser Int
      csename <- v .: "name"
      csepl   <- v .: "privacy_level"
      csest   <- v .: "scheduled_start_time"
      csedesc <- v .:? "description"
      cseimg  <- v .:? "image"

      case t of
        1 -> do
          csecid <- v .: "channel_id"
          cseet  <- v .:? "scheduled_end_time"
          return $ CreateScheduledEventDataStage csecid
                                                 csename
                                                 csepl
                                                 csest
                                                 cseet
                                                 csedesc
                                                 cseimg
        2 -> do
          csecid <- v .: "channel_id"
          cseet  <- v .:? "scheduled_end_time"
          return $ CreateScheduledEventDataVoice csecid
                                                 csename
                                                 csepl
                                                 csest
                                                 cseet
                                                 csedesc
                                                 cseimg
        3 -> do
          csemeta <- v .: "entity_metadata"
          cseloc  <- withObject "entity_metadata" (.: "location") csemeta
          cseet   <- v .: "scheduled_end_time"
          return $ CreateScheduledEventDataVoice cseloc
                                                 csename
                                                 csepl
                                                 csest
                                                 cseet
                                                 csedesc
                                                 cseimg
        _ -> error "unreachable"
    )


-- | The type of ScheduledEvent, used in 'ModifyScheduledEventData'
data ScheduledEventType
  = ScheduledEventTypeStage
  | ScheduledEventTypeVoice
  | ScheduledEventTypeExternal
  deriving (Show, Read, Ord, Eq, Data)

instance InternalDiscordEnum ScheduledEventType where
  discordTypeStartValue = ScheduledEventTypeStage
  fromDiscordType ScheduledEventTypeStage    = 1
  fromDiscordType ScheduledEventTypeVoice    = 2
  fromDiscordType ScheduledEventTypeExternal = 3

instance FromJSON ScheduledEventType where
  parseJSON = discordTypeParseJSON "ScheduledEventType"

instance ToJSON ScheduledEventType where
  toJSON = toJSON . fromDiscordType

-- | Data required to issue a Modify Scheduled Event request
-- This isnt fully type-safe, and can allow for boggus requests but I don't
-- know of any sane solution to this
data ModifyScheduledEventData = ModifyScheduledEventData
  { modifyScheduledEventDataChannelId    :: Maybe (Maybe ChannelId)
  , modifyScheduledEventDataLocation     :: Maybe (Maybe T.Text)
  , modifyScheduledEventDataName         :: Maybe T.Text
  , modifyScheduledEventDataPrivacyLevel :: Maybe ScheduledEventPrivacyLevel
  , modifyScheduledEventDataStartTime    :: Maybe UTCTime
  , modifyScheduledEventDataEndTime      :: Maybe UTCTime
  , modifyScheduledEventDataDescription  :: Maybe (Maybe T.Text)
  , modifyScheduledEventDataType         :: Maybe ScheduledEventType
  , modifyScheduledEventDataStatus       :: Maybe ScheduledEventStatus
  , modifyScheduledEventDataImage        :: Maybe CreateScheduledEventImage
  }

instance Default ModifyScheduledEventData where
  def = ModifyScheduledEventData Nothing
                                 Nothing
                                 Nothing
                                 Nothing
                                 Nothing
                                 Nothing
                                 Nothing
                                 Nothing
                                 Nothing
                                 Nothing

instance ToJSON ModifyScheduledEventData where
  toJSON ModifyScheduledEventData {..} = objectFromMaybes
      [ "channel_id"           .=? modifyScheduledEventDataChannelId
      , "entity_metadata"      .=? loc
      , "name"                 .=? modifyScheduledEventDataName
      , "scheduled_start_time" .=? modifyScheduledEventDataStartTime
      , "scheduled_end_time"   .=? modifyScheduledEventDataEndTime
      , "description"          .=? modifyScheduledEventDataDescription
      , "entity_type"          .=? modifyScheduledEventDataType
      , "status"               .=? modifyScheduledEventDataStatus
      , "image"                .=? modifyScheduledEventDataImage
      ]
   where
    loc = case modifyScheduledEventDataLocation of
      Nothing      -> Nothing
      Just Nothing -> Just Null
      Just loc'    -> Just $ object [("location", toJSON loc')]

instance FromJSON ModifyScheduledEventData where
  parseJSON = withObject
    "ModifyScheduledEventData"
    (\v -> do
      -- The trivial fields
      msename  <- v .:? "name"
      msest    <- v .:? "scheduled_start_time"
      mseet    <- v .:? "scheduled_end_time"
      msetype  <- v .:? "entity_type"
      msepl    <- v .:? "privacy_level"
      msestat  <- v .:? "status"
      mseimg   <- v .:? "image"

      -- The not so trivial ones
      msecid'  <- v .:! "channel_id"
      mseloc'  <- v .:! "entity_metadata"
      msedesc' <- v .:! "description"

      -- Extract the values
      msecid   <- case msecid' of
        Nothing   -> return Nothing
        Just Null -> return $ Just Nothing
        Just x    -> do
          x' <- parseJSON x
          return $ Just x'

      mseloc <- case mseloc' of
        Nothing   -> return Nothing
        Just Null -> return $ Just Nothing
        Just x    -> do
          x' <- withObject "entity_metadata" (.: "location") x
          return $ Just x'

      msedesc <- case msedesc' of
        Nothing   -> return Nothing
        Just Null -> return $ Just Nothing
        Just x    -> do
          x' <- parseJSON x
          return $ Just x'

      return $ ModifyScheduledEventData
        { modifyScheduledEventDataChannelId    = msecid
        , modifyScheduledEventDataLocation     = mseloc
        , modifyScheduledEventDataName         = msename
        , modifyScheduledEventDataPrivacyLevel = msepl
        , modifyScheduledEventDataStartTime    = msest
        , modifyScheduledEventDataEndTime      = mseet
        , modifyScheduledEventDataDescription  = msedesc
        , modifyScheduledEventDataType         = msetype
        , modifyScheduledEventDataStatus       = msestat
        , modifyScheduledEventDataImage        = mseimg
        }
    )

-- | An User that subscribed to a Scheduled Event
data ScheduledEventUser = ScheduledEventUser
  { scheduledEventUserEvent       :: ScheduledEventId
  , scheduledEventUserUser        :: User
  , scheduledEventUserGuildMember :: Maybe GuildMember
  }

instance FromJSON ScheduledEventUser where
  parseJSON = withObject
    "ScheduledEventUser"
    (\v ->
      ScheduledEventUser
        <$> v .:  "guild_scheduled_event_id"
        <*> v .:  "user"
        <*> v .:? "member"
    )
