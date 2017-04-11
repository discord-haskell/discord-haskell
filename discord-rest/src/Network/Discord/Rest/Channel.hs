{-# LANGUAGE GADTs, OverloadedStrings, InstanceSigs, TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
-- | Provides actions for Channel API interactions
module Network.Discord.Rest.Channel
  (
    ChannelRequest(..)
  ) where
   
    import Data.Aeson
    import Data.ByteString.Lazy
    import Data.Hashable
    import Data.Monoid (mempty, (<>))
    import Data.Text as T
    import Network.HTTP.Client (RequestBody (..))
    import Network.HTTP.Client.MultipartFormData (partFileRequestBody)
    import Network.HTTP.Req (reqBodyMultipart)
    
    import Network.Discord.Rest.Prelude
    import Network.Discord.Types
    import Network.Discord.Rest.HTTP

    -- | Data constructor for Channel requests. See <https://discordapp.com/developers/docs/resources/Channel Channel API>
    data ChannelRequest a where
      -- | Gets a channel by its id.
      GetChannel              :: Snowflake -> ChannelRequest Channel
      -- | Edits channels options.
      ModifyChannel           :: ToJSON a  => Snowflake -> a -> ChannelRequest Channel
      -- | Deletes a channel if its id doesn't equal to the id of guild.
      DeleteChannel           :: Snowflake -> ChannelRequest Channel
      -- | Gets a messages from a channel with limit of 100 per request.
      GetChannelMessages      :: Snowflake -> Range -> ChannelRequest [Message]
      -- | Gets a message in a channel by its id.
      GetChannelMessage       :: Snowflake -> Snowflake -> ChannelRequest Message
      -- | Sends a message to a channel.
      CreateMessage           :: Snowflake -> Text -> Maybe Embed -> ChannelRequest Message
      -- | Sends a message with a file to a channel.
      UploadFile              :: Snowflake -> FilePath -> ByteString -> ChannelRequest Message
      -- | Edits a message content.
      EditMessage             :: Message   -> Text -> Maybe Embed -> ChannelRequest Message
      -- | Deletes a message.
      DeleteMessage           :: Message   -> ChannelRequest ()
      -- | Deletes a group of messages.
      BulkDeleteMessage       :: Snowflake -> [Message] -> ChannelRequest ()
      -- | Edits a permission overrides for a channel.
      EditChannelPermissions  :: ToJSON a  => Snowflake -> Snowflake -> a -> ChannelRequest ()
      -- | Gets all instant invites to a channel.
      GetChannelInvites       :: Snowflake -> ChannelRequest Object
      -- | Creates an instant invite to a channel.
      CreateChannelInvite     :: ToJSON a  => Snowflake -> a -> ChannelRequest Object
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

    instance Hashable (ChannelRequest a) where
      hashWithSalt s (GetChannel chan) = hashWithSalt s ("get_chan"::Text, chan)
      hashWithSalt s (ModifyChannel chan _) = hashWithSalt s ("mod_chan"::Text, chan)
      hashWithSalt s (DeleteChannel chan) = hashWithSalt s ("mod_chan"::Text, chan)
      hashWithSalt s (GetChannelMessages chan _) = hashWithSalt s ("msg"::Text, chan)
      hashWithSalt s (GetChannelMessage chan _) = hashWithSalt s ("get_msg"::Text, chan)
      hashWithSalt s (CreateMessage chan _ _) = hashWithSalt s ("msg"::Text, chan)
      hashWithSalt s (UploadFile chan _ _)  = hashWithSalt s ("msg"::Text, chan)
      hashWithSalt s (EditMessage (Message _ chan _ _ _ _ _ _ _ _ _ _ _ _) _ _) =
        hashWithSalt s ("get_msg"::Text, chan)
      hashWithSalt s (DeleteMessage (Message _ chan _ _ _ _ _ _ _ _ _ _ _ _)) =
        hashWithSalt s ("get_msg"::Text, chan)
      hashWithSalt s (BulkDeleteMessage chan _) = hashWithSalt s ("del_msgs"::Text, chan)
      hashWithSalt s (EditChannelPermissions chan _ _) = hashWithSalt s ("perms"::Text, chan)
      hashWithSalt s (GetChannelInvites chan) = hashWithSalt s ("invites"::Text, chan)
      hashWithSalt s (CreateChannelInvite chan _) = hashWithSalt s ("invites"::Text, chan)
      hashWithSalt s (DeleteChannelPermission chan _) = hashWithSalt s ("perms"::Text, chan)
      hashWithSalt s (TriggerTypingIndicator chan)  = hashWithSalt s ("tti"::Text, chan)
      hashWithSalt s (GetPinnedMessages chan) = hashWithSalt s ("pins"::Text, chan)
      hashWithSalt s (AddPinnedMessage chan _) = hashWithSalt s ("pin"::Text, chan)
      hashWithSalt s (DeletePinnedMessage chan _) = hashWithSalt s ("pin"::Text, chan)

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
