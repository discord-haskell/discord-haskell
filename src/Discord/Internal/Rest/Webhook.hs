{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Provides actions for Webhook API interactions
module Discord.Internal.Rest.Webhook
  ( CreateWebhookOpts(..)
  , ExecuteWebhookWithTokenOpts(..)
  , ModifyWebhookOpts(..)
  , WebhookContent(..)
  , WebhookRequest(..)
  ) where

import           Data.Aeson
import qualified Data.Text as T
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import           Network.HTTP.Req ((/:), (/~))
import qualified Network.HTTP.Req as R
import Network.HTTP.Client (RequestBody (RequestBodyBS))
import Network.HTTP.Client.MultipartFormData (partBS, partFileRequestBody)

import Discord.Internal.Rest.Prelude
import Discord.Internal.Types

instance Request (WebhookRequest a) where
  majorRoute = webhookMajorRoute
  jsonRequest = webhookJsonRequest

-- | Data constructors for webhook requests.
data WebhookRequest a where
  -- | Creates a new webhook and returns a webhook object on success. Requires the @MANAGE_WEBHOOKS@ permission.
  -- An error will be returned if a webhook name (name) is not valid. A webhook name is valid if:
  --
  -- * It does not contain the substring @clyde@ (case-insensitive)
  -- * It follows the nickname guidelines in the Usernames and Nicknames documentation,
  --   with an exception that webhook names can be up to 80 characters
  CreateWebhook :: ChannelId
                -> CreateWebhookOpts
                -> WebhookRequest Webhook
  -- | Returns a channel's `Webhook`s as a list. Requires the @MANAGE_WEBHOOKS@ permission.
  GetChannelWebhooks :: ChannelId
                     -> WebhookRequest [Webhook]
  -- | Returns a guild's `Webhook`s as a list. Requires the @MANAGE_WEBHOOKS@ permission.
  GetGuildWebhooks :: GuildId
                   -> WebhookRequest [Webhook]
  -- | Returns the `Webhook` for the given id. If a token is given, authentication is not required.
  GetWebhook :: WebhookId
             -> Maybe WebhookToken
             -> WebhookRequest Webhook
  -- | Modify a webhook. Requires the @MANAGE_WEBHOOKS@ permission. Returns the updated `Webhook` on success.
  -- If a token is given, authentication is not required.
  ModifyWebhook :: WebhookId
                -> Maybe WebhookToken
                -> ModifyWebhookOpts
                -> WebhookRequest Webhook
  -- | Delete a webhook permanently. Requires the @MANAGE_WEBHOOKS@ permission.
  -- If a token is given, authentication is not required.
  DeleteWebhook :: WebhookId
                -> Maybe WebhookToken
                -> WebhookRequest ()
  -- | Executes a Webhook.
  -- 
  -- Refer to [Uploading Files](https://discord.com/developers/docs/reference#uploading-files)
  -- for details on attachments and @multipart/form-data@ requests.
  ExecuteWebhook :: WebhookId
                 -> WebhookToken
                 -> ExecuteWebhookWithTokenOpts
                 -> WebhookRequest ()
  -- We don't support slack and github compatible webhooks because you should
  --  just use execute webhook.

  -- | Returns a previously-sent webhook message from the same token.
  GetWebhookMessage :: WebhookId
                    -> WebhookToken
                    -> MessageId
                    -> WebhookRequest Message
  -- | Edits a previously-sent webhook message from the same token.
  EditWebhookMessage :: WebhookId
                     -> WebhookToken
                     -> MessageId
                     -> T.Text -- currently we don't support the full range of edits - feel free to PR and fix this
                     -> WebhookRequest Message
  -- | Deletes a previously-sent webhook message from the same token.
  DeleteWebhookMessage :: WebhookId
                       -> WebhookToken
                       -> MessageId
                       -> WebhookRequest ()

-- | Options for `ModifyWebhook` and `ModifyWebhookWithToken`
data ModifyWebhookOpts = ModifyWebhookOpts
  { modifyWebhookOptsName          :: Maybe T.Text
  , modifyWebhookOptsAvatar        :: Maybe T.Text
  , modifyWebhookOptsChannelId     :: Maybe ChannelId
  } deriving (Show, Read, Eq, Ord)

instance ToJSON ModifyWebhookOpts where
  toJSON ModifyWebhookOpts{..} = objectFromMaybes
                         ["channel_id" .=? modifyWebhookOptsChannelId,
                          "name" .=? modifyWebhookOptsName,
                          "avatar" .=? modifyWebhookOptsAvatar ]

-- | Options for `CreateWebhook`
data CreateWebhookOpts = CreateWebhookOpts
  { createWebhookOptsName          :: T.Text
  , createWebhookOptsAvatar        :: Maybe T.Text
  } deriving (Show, Read, Eq, Ord)

instance ToJSON CreateWebhookOpts where
  toJSON CreateWebhookOpts{..} = objectFromMaybes
                         ["name" .== createWebhookOptsName,
                          "avatar" .=? createWebhookOptsAvatar ]

-- | Options for `ExecuteWebhookWithToken`
data ExecuteWebhookWithTokenOpts = ExecuteWebhookWithTokenOpts
  { executeWebhookWithTokenOptsUsername      :: Maybe T.Text
  , executeWebhookWithTokenOptsContent       :: WebhookContent
  } deriving (Show, Read, Eq, Ord)

-- | A webhook's content
data WebhookContent = WebhookContentText T.Text
                    | WebhookContentFile T.Text B.ByteString
                    | WebhookContentEmbeds [CreateEmbed]
  deriving (Show, Read, Eq, Ord)

webhookContentJson :: WebhookContent -> [(AesonKey, Value)]
webhookContentJson c = case c of
                      WebhookContentText t -> [("content", toJSON t)]
                      WebhookContentFile _ _  -> []
                      WebhookContentEmbeds e -> [("embeds", toJSON (createEmbed <$> e))]

instance ToJSON ExecuteWebhookWithTokenOpts where
  toJSON ExecuteWebhookWithTokenOpts{..} = objectFromMaybes $
                          ["username" .=? executeWebhookWithTokenOptsUsername]
                           <> fmap Just (webhookContentJson executeWebhookWithTokenOptsContent)

-- | Major routes for webhook requests
webhookMajorRoute :: WebhookRequest a -> String
webhookMajorRoute ch = case ch of
  (CreateWebhook c _) ->    "aaaaaahook " <> show c
  (GetChannelWebhooks c) -> "aaaaaahook " <> show c
  (GetGuildWebhooks g) ->   "aaaaaahook " <> show g
  (GetWebhook w _) ->       "getwebhook " <> show w
  (ModifyWebhook w _ _) ->  "modifyhook " <> show w
  (DeleteWebhook w _) ->    "deletehook " <> show w
  (ExecuteWebhook w _ _) ->  "executehk " <> show w
  (GetWebhookMessage w _ _) -> "gethkmsg " <> show w
  (EditWebhookMessage w _ _ _) -> "edithkmsg " <> show w
  (DeleteWebhookMessage w _ _) -> "delhkmsg " <> show w

-- | Create a 'JsonRequest' from a `WebhookRequest`
webhookJsonRequest :: WebhookRequest r -> JsonRequest
webhookJsonRequest ch = case ch of
  (CreateWebhook channel patch) ->
    let body = pure (R.ReqBodyJson patch)
    in Post (baseUrl /: "channels" /~ channel /: "webhooks") body  mempty

  (GetChannelWebhooks c) ->
    Get (baseUrl /: "channels" /~ c /: "webhooks")  mempty

  (GetGuildWebhooks g) ->
    Get (baseUrl /: "guilds" /~ g /: "webhooks")  mempty

  (GetWebhook w t) ->
    Get (baseUrl /: "webhooks" /~ w /? t)  mempty

  (ModifyWebhook w t p) ->
    Patch (baseUrl /: "webhooks" /~ w /? t) (pure (R.ReqBodyJson p))  mempty

  (DeleteWebhook w t) ->
    Delete (baseUrl /: "webhooks" /~ w /? t)  mempty

  (ExecuteWebhook w tok o) ->
    case executeWebhookWithTokenOptsContent o of
      WebhookContentFile name text  ->
        let part = partFileRequestBody "file" (T.unpack name) (RequestBodyBS text)
            body = R.reqBodyMultipart [part]
        in Post (baseUrl /: "webhooks" /~ w /~ tok) body mempty
      WebhookContentText _ ->
        let body = pure (R.ReqBodyJson o)
        in Post (baseUrl /: "webhooks" /~ w /~ tok) body mempty
      WebhookContentEmbeds embeds ->
        let mkPart (name,content) = partFileRequestBody name (T.unpack name) (RequestBodyBS content)
            uploads CreateEmbed{..} = [(n,c) | (n, Just (CreateEmbedImageUpload c)) <-
                                          [ ("author.png", createEmbedAuthorIcon)
                                          , ("thumbnail.png", createEmbedThumbnail)
                                          , ("image.png", createEmbedImage)
                                          , ("footer.png", createEmbedFooterIcon) ]]
            parts =  map mkPart (concatMap uploads embeds)
            partsJson = [partBS "payload_json" $ BL.toStrict $ encode $ toJSON $ object ["embed" .= createEmbed e] | e <- embeds]
            body = R.reqBodyMultipart (partsJson ++ parts)
        in Post (baseUrl /: "webhooks" /~ w /: unToken tok) body mempty

  (GetWebhookMessage w t m) ->
    Get (baseUrl /: "webhooks" /~ w /~ t /: "messages" /~ m)  mempty

  (EditWebhookMessage w t m p) ->
    Patch (baseUrl /: "webhooks" /~ w /~ t /: "messages" /~ m) (pure (R.ReqBodyJson $ object ["content" .= p]))  mempty

  (DeleteWebhookMessage w t m) ->
    Delete (baseUrl /: "webhooks" /~ w /~ t /: "messages" /~ m)  mempty
