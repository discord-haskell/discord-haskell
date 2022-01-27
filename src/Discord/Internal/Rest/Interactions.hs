{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Discord.Internal.Rest.Interactions where

import Data.Aeson (encode)
import qualified Data.ByteString.Lazy as BL
import Discord.Internal.Rest.Prelude
import Discord.Internal.Types
import Discord.Internal.Types.Interactions
import Network.HTTP.Client.MultipartFormData (PartM, partBS)
import Network.HTTP.Req as R

data InteractionResponseRequest a where
  CreateInteractionResponse :: InteractionId -> InteractionToken -> InteractionResponse -> InteractionResponseRequest ()
  GetOriginalInteractionResponse :: ApplicationId -> InteractionToken -> InteractionResponseRequest Message
  EditOriginalInteractionResponse :: ApplicationId -> InteractionToken -> InteractionResponseMessage -> InteractionResponseRequest Message
  DeleteOriginalInteractionResponse :: ApplicationId -> InteractionToken -> InteractionResponseRequest ()
  CreateFollowupInteractionMessage :: ApplicationId -> InteractionToken -> InteractionResponseMessage -> InteractionResponseRequest Message
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
    Patch (interaction aid it /: "@original") (convertIRM i) mempty
  (DeleteOriginalInteractionResponse aid it) ->
    Delete (interaction aid it /: "@original") mempty
  (CreateFollowupInteractionMessage aid it i) ->
    Post (baseUrl /: "webhooks" // aid /: it) (convertIRM i) mempty
  (GetFollowupInteractionMessage aid it mid) ->
    Get (interaction aid it // mid) mempty
  (EditFollowupInteractionMessage aid it mid i) ->
    Patch (interaction aid it // mid) (convert i) mempty
  (DeleteFollowupInteractionMessage aid it mid) ->
    Delete (interaction aid it // mid) mempty
  where
    convert :: InteractionResponse -> RestIO ReqBodyMultipart
    convert ir@(InteractionResponseChannelMessage irm) = R.reqBodyMultipart (partBS "payload_json" (BL.toStrict $ encode ir) : convert' irm)
    convert ir@(InteractionResponseUpdateMessage irm) = R.reqBodyMultipart (partBS "payload_json" (BL.toStrict $ encode ir) : convert' irm)
    convert ir = R.reqBodyMultipart [partBS "payload_json" $ BL.toStrict $ encode ir]
    convertIRM :: InteractionResponseMessage -> RestIO ReqBodyMultipart
    convertIRM irm = R.reqBodyMultipart (partBS "payload_json" (BL.toStrict $ encode irm) : convert' irm)
    convert' :: InteractionResponseMessage -> [PartM IO]
    convert' InteractionResponseMessage {..} = case interactionResponseMessageEmbeds of
      Nothing -> []
      Just f -> (maybeEmbed . Just) =<< f
