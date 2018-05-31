{-# LANGUAGE GADTs, OverloadedStrings, InstanceSigs, TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings, DataKinds #-}
-- | Provides actions for Channel API interactions
module Network.Discord.Rest.Channel
  (
    ChannelRequest(..)
  ) where

import Data.Aeson
import qualified Data.ByteString.Lazy as BL
import Data.Hashable
import Data.Monoid (mempty, (<>))
import qualified Data.Text as T
import Network.HTTP.Client (RequestBody (..))
import Network.HTTP.Client.MultipartFormData (partFileRequestBody)
import Network.HTTP.Req (reqBodyMultipart, ReqBodyJson, (/:))
import qualified Network.HTTP.Req as R

import Network.Discord.Rest.Prelude
import Network.Discord.Types

-- | Data constructor for Channel requests. See <https://discordapp.com/developers/docs/resources/Channel Channel API>
data ChannelRequest a where
  -- | Gets a channel by its id.
  GetChannel              :: Snowflake -> ChannelRequest Channel
  -- | Edits channels options.
  ModifyChannel           :: ToJSON o  => Snowflake -> o -> ChannelRequest Channel
  -- | Deletes a channel if its id doesn't equal to the id of guild.
  DeleteChannel           :: Snowflake -> ChannelRequest Channel
  -- | Gets a messages from a channel with limit of 100 per request.
  GetChannelMessages      :: Snowflake -> Range -> ChannelRequest [Message]
  -- | Gets a message in a channel by its id.
  GetChannelMessage       :: Snowflake -> Snowflake -> ChannelRequest Message
  -- | Sends a message to a channel.
  CreateMessage           :: Snowflake -> T.Text -> Maybe Embed -> ChannelRequest Message
  -- | Sends a message with a file to a channel.
  UploadFile              :: Snowflake -> FilePath -> BL.ByteString -> ChannelRequest Message
  -- | Edits a message content.
  EditMessage             :: Message   -> T.Text -> Maybe Embed -> ChannelRequest Message
  -- | Deletes a message.
  DeleteMessage           :: Message   -> ChannelRequest ()
  -- | Deletes a group of messages.
  BulkDeleteMessage       :: Snowflake -> [Message] -> ChannelRequest ()
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

majorRouteChannel c = case c of
  (GetChannel chan) ->              "get_chan" <> T.pack (show chan)
  (ModifyChannel chan _) ->         "mod_chan" <> T.pack (show chan)
  (DeleteChannel chan) ->           "mod_chan" <> T.pack (show chan)
  (GetChannelMessages chan _) ->         "msg" <> T.pack (show chan)
  (GetChannelMessage chan _) ->      "get_msg" <> T.pack (show chan)
  (CreateMessage chan _ _) ->            "msg" <> T.pack (show chan)
  (UploadFile chan _ _) ->               "msg" <> T.pack (show chan)
  (EditMessage (Message _ chan _ _ _ _ _ _ _ _ _ _ _ _) _ _) ->
                                     "get_msg" <> T.pack (show chan)
  (DeleteMessage (Message _ chan _ _ _ _ _ _ _ _ _ _ _ _)) =
                                     "get_msg" <> T.pack (show chan)
  (BulkDeleteMessage (chan _)) ->   "del_msgs" <> T.pack (show chan)
  (EditChannelPermissions chan _ _) -> "perms" <> T.pack (show chan)
  (GetChannelInvites chan) ->        "invites" <> T.pack (show chan)
  (CreateChannelInvite chan _) ->    "invites" <> T.pack (show chan)
  (DeleteChannelPermission chan _) ->  "perms" <> T.pack (show chan)
  (TriggerTypingIndicator chan) ->       "tti" <> T.pack (show chan)
  (GetPinnedMessages chan) ->           "pins" <> T.pack (show chan)
  (AddPinnedMessage chan _) ->           "pin" <> T.pack (show chan)
  (DeletePinnedMessage chan _) ->        "pin" <> T.pack (show chan)

instance (FromJSON a) => DoFetch ChannelRequest a where
  doFetch req = go req
    where
      maybeEmbed :: Maybe Embed -> [(Text, Value)]
      maybeEmbed = maybe [] $ \embed -> ["embed" .= embed]

      url = baseUrl /: "channels"

      go :: DiscordRest m => ChannelRequest a -> m a
      go r@(GetChannel chan) = makeRequest r
        $ Get (url // chan) mempty
      go r@(ModifyChannel chan patch) = makeRequest r
        $ Patch (url // chan)
          (ReqBodyJson patch) mempty
      go r@(DeleteChannel chan) = makeRequest r
        $ Delete (url // chan) mempty
      go r@(GetChannelMessages chan range) = makeRequest r
        $ Get (url // chan /: "messages") (toQueryString range)
      go r@(GetChannelMessage chan msg) = makeRequest r
        $ Get (url // chan /: "messages" // msg) mempty
      go r@(CreateMessage chan msg embed) = makeRequest r
        $ Post (url // chan /: "messages")
          (ReqBodyJson . object $ ["content" .= msg] <> maybeEmbed embed)
          mempty
      go r@(UploadFile chan fileName file) = do
        body <- reqBodyMultipart [partFileRequestBody "file" fileName $ RequestBodyLBS file]
        makeRequest r $ Post (url // chan /: "messages")
          body mempty
      go r@(EditMessage (Message msg chan _ _ _ _ _ _ _ _ _ _ _ _) new embed) = makeRequest r
        $ Patch (url // chan /: "messages" // msg)
          (ReqBodyJson . object $ ["content" .= new] <> maybeEmbed embed)
          mempty
      go r@(DeleteMessage (Message msg chan _ _ _ _ _ _ _ _ _ _ _ _)) = makeRequest r
        $ Delete (url // chan /: "messages" // msg) mempty
      go r@(BulkDeleteMessage chan msgs) = makeRequest r
        $ Post (url // chan /: "messages" /: "bulk-delete")
          (ReqBodyJson $ object ["messages" .= Prelude.map messageId msgs])
          mempty
      go r@(EditChannelPermissions chan perm patch) = makeRequest r
        $ Put (url // chan /: "permissions" // perm)
          (ReqBodyJson patch) mempty
      go r@(GetChannelInvites chan) = makeRequest r
        $ Get (url // chan /: "invites") mempty
      go r@(CreateChannelInvite chan patch) = makeRequest r
        $ Post (url // chan /: "invites")
          (ReqBodyJson patch) mempty
      go r@(DeleteChannelPermission chan perm) = makeRequest r
        $ Delete (url // chan /: "permissions" // perm) mempty
      go r@(TriggerTypingIndicator chan) = makeRequest r
        $ Post (url // chan /: "typing")
          NoReqBody mempty
      go r@(GetPinnedMessages chan) = makeRequest r
        $ Get (url // chan /: "pins") mempty
      go r@(AddPinnedMessage chan msg) = makeRequest r
        $ Put (url // chan /: "pins" // msg)
          NoReqBody mempty
      go r@(DeletePinnedMessage chan msg) = makeRequest r
        $ Delete (url // chan /: "pins" // msg) mempty

