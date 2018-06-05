{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Provides actions for Channel API interactions
module Network.Discord.Rest.Channel
  (
    ChannelRequest(..)
  ) where

import Data.Aeson
import qualified Data.ByteString.Lazy as BL
import Data.Monoid (mempty, (<>))
import qualified Data.Text as T
import Network.HTTP.Client (RequestBody (..))
import Network.HTTP.Client.MultipartFormData (partFileRequestBody)
import Network.HTTP.Req ((/:))
import qualified Network.HTTP.Req as R

import Network.Discord.Rest.Prelude
import Network.Discord.Types


instance DiscordRequest ChannelRequest where
  majorRoute :: ChannelRequest a -> String
  majorRoute = majorRouteChannel

  createRequest :: FromJSON r => ChannelRequest r -> JsonRequest r
  createRequest = jsonRequestChannel


-- | Data constructor for Channel requests. See <https://discordapp.com/developers/docs/resources/Channel Channel API>
data ChannelRequest a where
  -- | Gets a channel by its id.
  GetChannel              :: Snowflake -> ChannelRequest Channel
  -- | Edits channels options.
  ModifyChannel           :: ToJSON o  => Snowflake -> o -> ChannelRequest Channel
  -- | Deletes a channel if its id doesn't equal to the id of guild.
  DeleteChannel           :: Snowflake -> ChannelRequest Channel
  -- | Gets a messages from a channel with limit of 100 per request.
  GetChannelMessages      :: Snowflake -> (Int, MessageTiming) -> ChannelRequest [Message]
  -- | Gets a message in a channel by its id.
  GetChannelMessage       :: Snowflake -> Snowflake -> ChannelRequest Message
  -- | Sends a message to a channel.
  CreateMessage           :: Snowflake -> T.Text -> Maybe Embed -> ChannelRequest Message
  -- | Sends a message with a file to a channel.
  UploadFile              :: Snowflake -> FilePath -> BL.ByteString -> ChannelRequest Message
  -- | Edits a message content.
  EditMessage             :: (Snowflake, Snowflake) -> T.Text -> Maybe Embed -> ChannelRequest Message
  -- | Deletes a message.
  DeleteMessage           :: (Snowflake, Snowflake) -> ChannelRequest ()
  -- | Deletes a group of messages.
  BulkDeleteMessage       :: (Snowflake, [Snowflake]) -> ChannelRequest ()
  -- | Edits a permission overrides for a channel.
  EditChannelPermissions  :: ToJSON o  => Snowflake -> Snowflake -> o -> ChannelRequest ()
  -- | Gets all instant invites to a channel.
  GetChannelInvites       :: Snowflake -> ChannelRequest Object
  -- | Creates an instant invite to a channel.
  CreateChannelInvite     :: ToJSON o  => Snowflake -> o -> ChannelRequest Object
  -- | Deletes a permission override from a channel.
  DeleteChannelPermission :: Snowflake -> Snowflake -> ChannelRequest ()
  -- | Sends a typing indicator a channel which lasts 10 seconds.
  TriggerTypingIndicator  :: Snowflake -> ChannelRequest ()
  -- | Gets all pinned messages of a channel.
  GetPinnedMessages       :: Snowflake -> ChannelRequest [Message]
  -- | Pins a message.
  AddPinnedMessage        :: Snowflake -> Snowflake -> ChannelRequest ()
  -- | Unpins a message.
  DeletePinnedMessage     :: Snowflake -> Snowflake -> ChannelRequest ()


-- | Data constructor for GetChannelMessages requests. See <https://discordapp.com/developers/docs/resources/channel#get-channel-messages>
data MessageTiming = Around Snowflake
                   | Before Snowflake
                   | After Snowflake

timingToQuery :: R.QueryParam p => MessageTiming -> p
timingToQuery t = case t of
  (Around snow) -> "around" R.=: show snow
  (Before snow) -> "before" R.=: show snow
  (After  snow) -> "after"  R.=: show snow

majorRouteChannel :: ChannelRequest a -> String
majorRouteChannel c = case c of
  (GetChannel chan) ->              "get_chan " <> show chan
  (ModifyChannel chan _) ->         "mod_chan " <> show chan
  (DeleteChannel chan) ->           "mod_chan " <> show chan
  (GetChannelMessages chan _) ->         "msg " <> show chan
  (GetChannelMessage chan _) ->      "get_msg " <> show chan
  (CreateMessage chan _ _) ->            "msg " <> show chan
  (UploadFile chan _ _) ->               "msg " <> show chan
  (EditMessage (chan, _) _ _) ->     "get_msg " <> show chan
  (DeleteMessage (chan, _)) ->       "get_msg " <> show chan
  (BulkDeleteMessage (chan, _)) ->  "del_msgs " <> show chan
  (EditChannelPermissions chan _ _) -> "perms " <> show chan
  (GetChannelInvites chan) ->        "invites " <> show chan
  (CreateChannelInvite chan _) ->    "invites " <> show chan
  (DeleteChannelPermission chan _) ->  "perms " <> show chan
  (TriggerTypingIndicator chan) ->       "tti " <> show chan
  (GetPinnedMessages chan) ->           "pins " <> show chan
  (AddPinnedMessage chan _) ->           "pin " <> show chan
  (DeletePinnedMessage chan _) ->        "pin " <> show chan

maybeEmbed :: Maybe Embed -> [(T.Text, Value)]
maybeEmbed = maybe [] $ \embed -> ["embed" .= embed]

url :: R.Url 'R.Https
url = baseUrl /: "channels"

jsonRequestChannel :: FromJSON r => ChannelRequest r -> JsonRequest r
jsonRequestChannel c = case c of
  (GetChannel chan) -> Get (url // chan) mempty
  (ModifyChannel chan patch) -> Patch (url // chan) (R.ReqBodyJson patch) mempty
  (DeleteChannel chan) -> Delete (url // chan) mempty
  (GetChannelMessages chan (n,timing)) -> Get (url // chan /: "messages") ("limit" R.=: n <> timingToQuery timing)
  (GetChannelMessage chan msg) -> Get (url // chan /: "messages" // msg) mempty
  (CreateMessage chan msg embed) -> Post (url // chan /: "messages")
   (pure . R.ReqBodyJson . object $ ["content" .= msg] <> maybeEmbed embed) mempty
  (UploadFile chan fileName file) ->
     let body = R.reqBodyMultipart [partFileRequestBody "file" fileName $ RequestBodyLBS file]
     in Post (url // chan /: "messages") body mempty
  (EditMessage (chan, msg) new embed) ->
                   Patch (url // chan /: "messages" // msg)
                      (R.ReqBodyJson . object $ ["content" .= new] <> maybeEmbed embed) mempty
  (DeleteMessage (chan, msg)) ->
                   Delete (url // chan /: "messages" // msg) mempty
  (BulkDeleteMessage (chan, msgs)) -> Post (url // chan /: "messages" /: "bulk-delete")
                       (pure . R.ReqBodyJson $ object ["messages" .= msgs]) mempty
  (EditChannelPermissions chan perm patch) -> Put (url // chan /: "permissions" // perm)
                       (R.ReqBodyJson patch) mempty
  (GetChannelInvites chan) -> Get (url // chan /: "invites") mempty
  (CreateChannelInvite chan patch) -> Post (url // chan /: "invites") (pure (R.ReqBodyJson patch)) mempty
  (DeleteChannelPermission chan perm) -> Delete (url // chan /: "permissions" // perm) mempty
  (TriggerTypingIndicator chan) -> Post (url // chan /: "typing") (pure R.NoReqBody) mempty
  (GetPinnedMessages chan) -> Get (url // chan /: "pins") mempty
  (AddPinnedMessage chan msg) -> Put (url // chan /: "pins" // msg) R.NoReqBody mempty
  (DeletePinnedMessage chan msg) -> Delete (url // chan /: "pins" // msg) mempty

