{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Discord.Internal.Rest.Polls
  ( PollRequest (..),
    PollTiming (..),
    PollRequestResult (..),
  )
where

import Data.Aeson
import qualified Data.Text as T ( pack )
import Discord.Internal.Rest.Prelude
import Discord.Internal.Types
import Network.HTTP.Req ((/:), (/~))
import qualified Network.HTTP.Req as R

instance Request (PollRequest a) where
  majorRoute = pollMajorRoute
  jsonRequest = pollJsonRequest

data PollRequest a where
  -- | Get a list of users who have voted for a specific answer
  GetAnswerVoters :: (ChannelId, MessageId) -> Int -> PollTiming -> PollRequest PollRequestResult
  -- | End a poll that belongs to the current user
  EndPoll :: (ChannelId, MessageId) -> PollRequest Message

-- not the best way to go about this, hopefully a better solution is found
newtype PollRequestResult = PollRequestResult [User]
  deriving (Show, Read, Eq, Ord)

instance FromJSON PollRequestResult where
  parseJSON = withObject "PollRequestResult" $ \o -> do
    users <- o .: "users"
    pure $ PollRequestResult users

-- copied from GetGuildMembers implementation of timing
data PollTiming = PollTiming
  { pollTimingLimit :: Maybe Int,
    pollTimingAfter :: Maybe UserId
  }
  deriving (Show, Read, Eq, Ord)

pollTimingToQuery :: PollTiming -> R.Option 'R.Https
pollTimingToQuery (PollTiming mLimit mAfter) =
  let limit = case mLimit of
        Nothing -> mempty
        Just lim -> "limit" R.=: lim
      after = case mAfter of
        Nothing -> mempty
        Just aft -> "after" R.=: show aft
   in limit <> after

pollMajorRoute :: PollRequest a -> String
pollMajorRoute = \case
  (GetAnswerVoters (c, _) _ _) -> "poll " <> show c
  (EndPoll (c, _)) -> "poll " <> show c

pollJsonRequest :: PollRequest r -> JsonRequest
pollJsonRequest = \case
  (GetAnswerVoters (chan, msgid) aid timing) -> -- TODO: T.show _ / T.pack (show _) / find good way to go about this
    Get (baseUrl /: "channels" /~ chan /: "polls" /~ msgid /: "answers" /: T.pack (show aid)) (pollTimingToQuery timing)
  (EndPoll (chan, msgid)) ->
    Post (baseUrl /: "channels" /~ chan /: "polls" /~ msgid /: "expire") (pure R.NoReqBody) mempty
