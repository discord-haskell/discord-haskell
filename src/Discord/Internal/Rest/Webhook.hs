{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Provides actions for Webhook API interactions
module Discord.Internal.Rest.Webhook
  ( CreateWebhookOpts (..),
    ExecuteWebhookWithTokenOpts (..),
    ModifyWebhookOpts (..),
    WebhookContent (..),
    WebhookRequest (..),
  )
where

import Data.Aeson
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import Discord.Internal.Rest.Prelude
import Discord.Internal.Types
import Network.HTTP.Client (RequestBody (RequestBodyBS))
import Network.HTTP.Client.MultipartFormData (partBS, partFileRequestBody)
import Network.HTTP.Req ((/:))
import qualified Network.HTTP.Req as R

-- aeson introduced type name for json key (text)
-- https://github.com/haskell/aeson/issues/881
# if MIN_VERSION_aeson(2, 0, 0)
import qualified Data.Aeson.Key as Key
toKey :: T.Text -> Key.Key
toKey = Key.fromText
# else
toKey :: T.Text -> T.Text
toKey = id
# endif

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
  CreateWebhook ::
    ChannelId ->
    CreateWebhookOpts ->
    WebhookRequest Webhook
  -- | Returns a channel's `Webhook`s as a list. Requires the @MANAGE_WEBHOOKS@ permission.
  GetChannelWebhooks ::
    ChannelId ->
    WebhookRequest [Webhook]
  -- | Returns a guild's `Webhook`s as a list. Requires the @MANAGE_WEBHOOKS@ permission.
  GetGuildWebhooks ::
    GuildId ->
    WebhookRequest [Webhook]
  -- | Returns the `Webhook` for the given id.
  GetWebhook ::
    WebhookId ->
    WebhookRequest Webhook
  -- | Same as `GetWebhook`, except this call does not require authentication.
  GetWebhookWithToken ::
    WebhookId ->
    T.Text ->
    WebhookRequest Webhook
  -- | Modify a webhook. Requires the @MANAGE_WEBHOOKS@ permission. Returns the updated `Webhook` on success.
  ModifyWebhook ::
    WebhookId ->
    ModifyWebhookOpts ->
    WebhookRequest Webhook
  -- | Same as `ModifyWebhook`, except this call does not require authentication.
  ModifyWebhookWithToken ::
    WebhookId ->
    T.Text ->
    ModifyWebhookOpts ->
    WebhookRequest Webhook
  -- | Delete a webhook permanently. Requires the @MANAGE_WEBHOOKS@ permission.
  DeleteWebhook ::
    WebhookId ->
    WebhookRequest ()
  -- | Same as `DeleteWebhook`, except this call does not require authentication.
  DeleteWebhookWithToken ::
    WebhookId ->
    T.Text ->
    WebhookRequest ()
  -- | Executes a Webhook.
  --
  -- Refer to [Uploading Files](https://discord.com/developers/docs/reference#uploading-files)
  -- for details on attachments and @multipart/form-data@ requests.
  ExecuteWebhookWithToken ::
    WebhookId ->
    T.Text ->
    ExecuteWebhookWithTokenOpts ->
    WebhookRequest ()

-- | Options for `ModifyWebhook` and `ModifyWebhookWithToken`
data ModifyWebhookOpts = ModifyWebhookOpts
  { modifyWebhookOptsName :: Maybe T.Text,
    modifyWebhookOptsAvatar :: Maybe T.Text,
    modifyWebhookOptsChannelId :: Maybe ChannelId
  }
  deriving (Show, Read, Eq, Ord)

instance ToJSON ModifyWebhookOpts where
  toJSON ModifyWebhookOpts {..} =
    object
      [ (toKey name, val)
        | (name, Just val) <-
            [ ("channel_id", toJSON <$> modifyWebhookOptsChannelId),
              ("name", toJSON <$> modifyWebhookOptsName),
              ("avatar", toJSON <$> modifyWebhookOptsAvatar)
            ]
      ]

-- | Options for `CreateWebhook`
data CreateWebhookOpts = CreateWebhookOpts
  { createWebhookOptsName :: T.Text,
    createWebhookOptsAvatar :: Maybe T.Text
  }
  deriving (Show, Read, Eq, Ord)

instance ToJSON CreateWebhookOpts where
  toJSON CreateWebhookOpts {..} =
    object
      [ (name, val)
        | (name, Just val) <-
            [ ("name", toJSON <$> Just createWebhookOptsName),
              ("avatar", toJSON <$> createWebhookOptsAvatar)
            ]
      ]

-- | Options for `ExecuteWebhookWithToken`
data ExecuteWebhookWithTokenOpts = ExecuteWebhookWithTokenOpts
  { executeWebhookWithTokenOptsUsername :: Maybe T.Text,
    executeWebhookWithTokenOptsContent :: WebhookContent
  }
  deriving (Show, Read, Eq, Ord)

-- | A webhook's content
data WebhookContent
  = WebhookContentText T.Text
  | WebhookContentFile T.Text B.ByteString
  | WebhookContentEmbeds [CreateEmbed]
  deriving (Show, Read, Eq, Ord)

webhookContentJson :: WebhookContent -> [(T.Text, Maybe Value)]
webhookContentJson c = case c of
  WebhookContentText t -> [("content", Just (toJSON t))]
  WebhookContentFile _ _ -> []
  WebhookContentEmbeds e -> [("embeds", Just (toJSON (createEmbed <$> e)))]

instance ToJSON ExecuteWebhookWithTokenOpts where
  toJSON ExecuteWebhookWithTokenOpts {..} =
    object $
      [ (toKey name, val)
        | (name, Just val) <-
            [("username", toJSON <$> executeWebhookWithTokenOptsUsername)]
              <> webhookContentJson executeWebhookWithTokenOptsContent
      ]

-- | Major routes for webhook requests
webhookMajorRoute :: WebhookRequest a -> String
webhookMajorRoute ch = case ch of
  (CreateWebhook c _) -> "aaaaaahook " <> show c
  (GetChannelWebhooks c) -> "aaaaaahook " <> show c
  (GetGuildWebhooks g) -> "aaaaaahook " <> show g
  (GetWebhook w) -> "aaaaaahook " <> show w
  (GetWebhookWithToken w _) -> "getwebhook " <> show w
  (ModifyWebhook w _) -> "modifyhook " <> show w
  (ModifyWebhookWithToken w _ _) -> "modifyhook " <> show w
  (DeleteWebhook w) -> "deletehook " <> show w
  (DeleteWebhookWithToken w _) -> "deletehook " <> show w
  (ExecuteWebhookWithToken w _ _) -> "executehk " <> show w

-- | Create a 'JsonRequest' from a `WebhookRequest`
webhookJsonRequest :: WebhookRequest r -> JsonRequest
webhookJsonRequest ch = case ch of
  (CreateWebhook channel patch) ->
    let body = pure (R.ReqBodyJson patch)
     in Post (baseUrl /: "channels" // channel /: "webhooks") body mempty
  (GetChannelWebhooks c) ->
    Get (baseUrl /: "channels" // c /: "webhooks") mempty
  (GetGuildWebhooks g) ->
    Get (baseUrl /: "guilds" // g /: "webhooks") mempty
  (GetWebhook w) ->
    Get (baseUrl /: "webhooks" // w) mempty
  (GetWebhookWithToken w t) ->
    Get (baseUrl /: "webhooks" // w /: t) mempty
  (ModifyWebhook w patch) ->
    Patch (baseUrl /: "webhooks" // w) (pure (R.ReqBodyJson patch)) mempty
  (ModifyWebhookWithToken w t p) ->
    Patch (baseUrl /: "webhooks" // w /: t) (pure (R.ReqBodyJson p)) mempty
  (DeleteWebhook w) ->
    Delete (baseUrl /: "webhooks" // w) mempty
  (DeleteWebhookWithToken w t) ->
    Delete (baseUrl /: "webhooks" // w /: t) mempty
  (ExecuteWebhookWithToken w tok o) ->
    case executeWebhookWithTokenOptsContent o of
      WebhookContentFile name text ->
        let part = partFileRequestBody "file" (T.unpack name) (RequestBodyBS text)
            body = R.reqBodyMultipart [part]
         in Post (baseUrl /: "webhooks" // w /: tok) body mempty
      WebhookContentText _ ->
        let body = pure (R.ReqBodyJson o)
         in Post (baseUrl /: "webhooks" // w /: tok) body mempty
      WebhookContentEmbeds embeds ->
        let mkPart (name, content) = partFileRequestBody name (T.unpack name) (RequestBodyBS content)
            uploads CreateEmbed {..} =
              [ (n, c)
                | (n, Just (CreateEmbedImageUpload c)) <-
                    [ ("author.png", createEmbedAuthorIcon),
                      ("thumbnail.png", createEmbedThumbnail),
                      ("image.png", createEmbedImage),
                      ("footer.png", createEmbedFooterIcon)
                    ]
              ]
            parts = map mkPart (concatMap uploads embeds)
            partsJson = [partBS "payload_json" $ BL.toStrict $ encode $ toJSON $ object ["embed" .= createEmbed e] | e <- embeds]
            body = R.reqBodyMultipart (partsJson ++ parts)
         in Post (baseUrl /: "webhooks" // w /: tok) body mempty
