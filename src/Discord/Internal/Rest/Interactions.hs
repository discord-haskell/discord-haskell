{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}

module Discord.Internal.Rest.Interactions (InteractionResponseRequest(..)) where

import Data.Aeson (encode)
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as BS
import Discord.Internal.Rest.Prelude
    ( RestIO,
      Request(..),
      JsonRequest(Delete, Post, Get, Patch),
      baseUrl)
import Discord.Internal.Types
import Discord.Internal.Types.Interactions
import Network.HTTP.Client.MultipartFormData (PartM, partBS)
import Network.HTTP.Req ((/:), (/~))
import qualified Network.HTTP.Req as R
import Data.Functor ((<&>))
import qualified Data.Aeson.KeyMap as A.KM
import Data.Bifunctor (first)

-- | Data constructor for Interaction response requests
data InteractionResponseRequest a where
  -- | Create a response to an Interaction from the gateway.
  --
  -- This endpoint also supports file attachments similar to the webhook endpoints.
  -- Refer to [Uploading files](https://discord.com/developers/docs/reference#uploading-files)
  -- for details on uploading files and @multipart/form-data@ requests.
  CreateInteractionResponse :: InteractionId -> InteractionToken -> InteractionResponse -> InteractionResponseRequest ()
  -- | Returns the initial Interaction response.
  GetOriginalInteractionResponse :: ApplicationId -> InteractionToken -> InteractionResponseRequest Message
  -- | Edits the initial Interaction response.
  EditOriginalInteractionResponse :: ApplicationId -> InteractionToken -> InteractionResponseMessage -> InteractionResponseRequest Message
  -- | Deletes the initial Interaction response.
  DeleteOriginalInteractionResponse :: ApplicationId -> InteractionToken -> InteractionResponseRequest ()
  -- | Create a followup message for an Interaction
  CreateFollowupInteractionMessage :: ApplicationId -> InteractionToken -> InteractionResponseMessage -> InteractionResponseRequest Message
  -- | Returns a followup message for an Interaction.
  GetFollowupInteractionMessage :: ApplicationId -> InteractionToken -> MessageId -> InteractionResponseRequest Message
  -- | Edits a followup message for an Interaction.
  EditFollowupInteractionMessage :: ApplicationId -> InteractionToken -> MessageId -> InteractionResponse -> InteractionResponseRequest Message
  -- | Deletes a followup message for an Interaction.
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
interaction aid it = baseUrl /: "webhooks" /~ aid /~ it /: "messages"

interactionResponseJsonRequest :: InteractionResponseRequest a -> JsonRequest
interactionResponseJsonRequest a = case a of
  (CreateInteractionResponse iid it i) ->
    Post (baseUrl /: "interactions" /~ iid /~ it /: "callback") (convert i) mempty
  (GetOriginalInteractionResponse aid it) ->
    Get (interaction aid it /: "@original") mempty
  (EditOriginalInteractionResponse aid it i) ->
    Patch (interaction aid it /: "@original") (convertIRM i) mempty
  (DeleteOriginalInteractionResponse aid it) ->
    Delete (interaction aid it /: "@original") mempty
  (CreateFollowupInteractionMessage aid it i) ->
    Post (baseUrl /: "webhooks" /~ aid /~ it) (convertIRM i) mempty
  (GetFollowupInteractionMessage aid it mid) ->
    Get (interaction aid it /~ mid) mempty
  (EditFollowupInteractionMessage aid it mid i) ->
    Patch (interaction aid it /~ mid) (convert i) mempty
  (DeleteFollowupInteractionMessage aid it mid) ->
    Delete (interaction aid it /~ mid) mempty
  where
    convert :: InteractionResponse -> RestIO R.ReqBodyMultipart
    convert ir@(InteractionResponseChannelMessage irm) =
      let (attachmentF, binaryData) = embedAttachments irm in
      R.reqBodyMultipart (partBS "payload_json" (attachmentF ir) : binaryData)
    convert ir@(InteractionResponseUpdateMessage irm) = 
      let (attachmentF, binaryData) = embedAttachments irm in
      R.reqBodyMultipart (partBS "payload_json" (attachmentF ir) : binaryData)
    convert ir = R.reqBodyMultipart [partBS "payload_json" $ BL.toStrict $ encode ir]
    convertIRM :: InteractionResponseMessage -> RestIO R.ReqBodyMultipart
    convertIRM irm = 
      let (attachmentF, binaryData) = embedAttachments irm in
      R.reqBodyMultipart (partBS "payload_json" (attachmentF irm) : binaryData)

    -- we kinda inject the attachments list into the Value produced.
    embedAttachments :: ToJSON a => InteractionResponseMessage -> (a -> BS.ByteString, [PartM IO])
    embedAttachments irm = first (((BL.toStrict . encode) .) . (. toJSON)) $
      case embedAttachments' irm of
        Nothing -> (id, [])
        Just (unzip -> (attachments, datas)) ->
          (onObject (A.KM.insert "attachments" (toJSON attachments)), datas)
      where
      embedAttachments' :: InteractionResponseMessage -> Maybe [(BasicAttachment, PartM IO)]
      embedAttachments' InteractionResponseMessage {..} =
        interactionResponseMessageEmbeds <&> embedsToAttachments 0

      onObject f = \case
        A.Object o -> A.Object $ f o
        v -> v
