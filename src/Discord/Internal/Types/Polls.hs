{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Discord.Internal.Types.Polls where

import Data.Aeson
import Data.Text (Text)
import Data.Time.Clock
import Discord.Internal.Types.Emoji (Emoji)
import Discord.Internal.Types.Prelude

data Poll = Poll
  { pollQuestion :: PollMedia,
    pollAnswers :: [PollAnswer],
    pollExpiry :: UTCTime,
    pollAllowMultiselect :: Bool,
    pollLayoutType :: Int,
    pollResults :: Maybe PollResults
  }
  deriving (Eq, Show, Read, Ord)

instance FromJSON Poll where
  parseJSON = withObject "Poll" $ \o ->
    Poll
      <$> o .: "question"
      <*> o .: "answers"
      <*> o .: "expiry"
      <*> o .: "allow_multiselect"
      <*> o .: "layout_type"
      <*> o .:? "results"

instance ToJSON Poll where
  toJSON Poll {..} =
    objectFromMaybes
      [ "question" .== pollQuestion,
        "answers" .== pollAnswers,
        "expiry" .== pollExpiry,
        "allow_multiselect" .== pollAllowMultiselect,
        "layout_type" .== pollLayoutType,
        "results" .=? pollResults
      ]

data PollCreateRequest = PollCreateRequest
  { pollCreateQuestion :: PollMedia,
    pollCreateAnswers :: [PollAnswer],
    pollCreateDuration :: Maybe Int,
    pollCreateAllowMultiselect :: Maybe Bool,
    pollCreateLayoutType :: Maybe Int
  }
  deriving (Eq, Show, Read, Ord)

instance FromJSON PollCreateRequest where
  parseJSON = withObject "PollCreateRequest" $ \o ->
    PollCreateRequest
      <$> o .: "question"
      <*> o .: "answers"
      <*> o .:? "duration"
      <*> o .:? "allow_multiselect"
      <*> o .:? "layout_type"

instance ToJSON PollCreateRequest where
  toJSON PollCreateRequest {..} =
    objectFromMaybes
      [ "question" .== pollCreateQuestion,
        "answers" .== pollCreateAnswers,
        "duration" .=? pollCreateDuration,
        "allow_multiselect" .=? pollCreateAllowMultiselect,
        "layout_type" .=? pollCreateLayoutType
      ]

data PollAnswer = PollAnswer
  { pollAnswerId :: Int,
    pollAnswerMedia :: PollMedia
  }
  deriving (Eq, Show, Read, Ord)

instance FromJSON PollAnswer where
  parseJSON = withObject "PollAnswer" $ \o ->
    PollAnswer
      <$> o .: "answer_id"
      <*> o .: "poll_media"

instance ToJSON PollAnswer where
  toJSON PollAnswer {..} =
    objectFromMaybes
      [ "answer_id" .== pollAnswerId,
        "poll_media" .== pollAnswerMedia
      ]

data PollMedia = PollMedia
  { pollMediaText :: Text,
    pollMediaEmoji :: Maybe Emoji
  }
  deriving (Eq, Show, Read, Ord)

instance FromJSON PollMedia where
  parseJSON = withObject "PollMedia" $ \o ->
    PollMedia
      <$> o .: "text"
      <*> o .:? "emoji"

instance ToJSON PollMedia where
  toJSON PollMedia {..} =
    objectFromMaybes
      [ "text" .== pollMediaText,
        "emoji" .=? pollMediaEmoji
      ]

data PollResults = PollResults
  { pollResultsIsFinalized :: Bool,
    pollResultsAnswerCounts :: [PollAnswerCount]
  }
  deriving (Eq, Show, Read, Ord)

instance FromJSON PollResults where
  parseJSON = withObject "PollResults" $ \o ->
    PollResults
      <$> o .: "is_finalized"
      <*> o .: "answer_counts"

instance ToJSON PollResults where
  toJSON PollResults {..} =
    objectFromMaybes
      [ "is_finalized" .== pollResultsIsFinalized,
        "answer_counts" .== pollResultsAnswerCounts
      ]

data PollAnswerCount = PollAnswerCount
  { pollAnswerCountId :: Int,
    pollAnswerCount :: Int,
    pollAnswerMeVoted :: Bool
  }
  deriving (Eq, Show, Read, Ord)

instance FromJSON PollAnswerCount where
  parseJSON = withObject "PollAnswerCount" $ \o ->
    PollAnswerCount
      <$> o .: "id"
      <*> o .: "count"
      <*> o .: "me_voted"

instance ToJSON PollAnswerCount where
  toJSON PollAnswerCount {..} =
    objectFromMaybes
      [ "id" .== pollAnswerCountId,
        "count" .== pollAnswerCount,
        "me_voted" .== pollAnswerMeVoted
      ]
