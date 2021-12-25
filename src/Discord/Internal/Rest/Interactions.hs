{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Discord.Internal.Rest.Interactions where

import Data.Aeson (ToJSON (toJSON), Value)
import Discord.Internal.Rest.Prelude (JsonRequest (..), Request (..), RestIO, baseUrl, (//))
import Discord.Internal.Types (ApplicationId, InteractionId, InteractionResponse, InteractionToken, Message, MessageId, InteractionCallbackMessages)
import Network.HTTP.Req as R

data InteractionResponseRequest a where
  CreateInteractionResponse :: InteractionId -> InteractionToken -> InteractionResponse -> InteractionResponseRequest ()
  GetOriginalInteractionResponse :: ApplicationId -> InteractionToken -> InteractionResponseRequest Message
  EditOriginalInteractionResponse :: ApplicationId -> InteractionToken -> InteractionCallbackMessages -> InteractionResponseRequest Message
  DeleteOriginalInteractionResponse :: ApplicationId -> InteractionToken -> InteractionResponseRequest ()
  CreateFollowupInteractionMessage :: ApplicationId -> InteractionToken -> InteractionCallbackMessages  -> InteractionResponseRequest Message
  GetFollowupInteractionMessage :: ApplicationId -> InteractionToken -> MessageId -> InteractionResponseRequest Message
  EditFollowupInteractionMessage :: ApplicationId -> InteractionToken -> MessageId -> InteractionResponse -> InteractionResponseRequest Message
  DeleteFollowupInteractionMessage :: ApplicationId -> InteractionToken -> MessageId -> InteractionResponseRequest ()

instance Request (InteractionResponseRequest a) where
  jsonRequest = interactionResponseJsonRequest
  majorRoute = interactionResponseMajorRoute

interactionResponseMajorRoute :: InteractionResponseRequest a -> String
interactionResponseMajorRoute a = case a of
  (CreateInteractionResponse iid _ _) -> "intresp " <> show iid
  (GetOriginalInteractionResponse aid _) -> "intresp " <> show aid
  (EditOriginalInteractionResponse aid _ _) -> "intresp " <> show aid
  (DeleteOriginalInteractionResponse aid _) -> "intresp " <> show aid
  (CreateFollowupInteractionMessage iid _ _) -> "intrespf " <> show iid
  (GetFollowupInteractionMessage aid _ _) -> "intrespf " <> show aid
  (EditFollowupInteractionMessage aid _ _ _) -> "intrespf " <> show aid
  (DeleteFollowupInteractionMessage aid _ _) -> "intrespf " <> show aid

interaction :: ApplicationId -> InteractionToken -> R.Url 'R.Https
interaction aid it = baseUrl /: "webhooks" // aid /: it /: "messages"

interactionResponseJsonRequest :: InteractionResponseRequest a -> JsonRequest
interactionResponseJsonRequest a = case a of
  (CreateInteractionResponse iid it i) ->
    Post (baseUrl /: "interactions" // iid /: it /: "callback") (convert i) mempty
  (GetOriginalInteractionResponse aid it) ->
    Get (interaction aid it /: "@original") mempty
  (EditOriginalInteractionResponse aid it i) ->
    Patch (interaction aid it /: "@original") (convert i) mempty
  (DeleteOriginalInteractionResponse aid it) ->
    Delete (interaction aid it /: "@original") mempty
  (CreateFollowupInteractionMessage aid it i) ->
    Post (baseUrl /: "webhooks" // aid /: it) (convert i) mempty
  (GetFollowupInteractionMessage aid it mid) ->
    Get (interaction aid it // mid) mempty
  (EditFollowupInteractionMessage aid it mid i) ->
    Patch (interaction aid it // mid) (convert i) mempty
  (DeleteFollowupInteractionMessage aid it mid) ->
    Delete (interaction aid it // mid) mempty
  where
    convert :: (ToJSON a) => a -> RestIO (ReqBodyJson Value)
    convert = (pure @RestIO) . R.ReqBodyJson . toJSON
