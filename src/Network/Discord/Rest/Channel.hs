{-# LANGUAGE GADTs, OverloadedStrings, InstanceSigs, TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
module Network.Discord.Rest.Channel
  (
    ChannelRequest(..)
  ) where
    import Data.Text
    import Data.ByteString.Lazy
    import qualified Control.Monad.State as ST (get, liftIO)
    import Control.Monad.Morph (lift)
    import Control.Monad (when)
    import Control.Concurrent.STM

    import Data.Aeson
    import Data.Hashable
    import Network.Wreq
    import Control.Lens
    import Data.Time.Clock.POSIX

    import Network.Discord.Types as Dc
    import Network.Discord.Rest.Prelude

    -- | Data constructor for Channel requests. See <https://discordapp.com/developers/docs/resources/Channel Channel API>
    data ChannelRequest a where
      -- | Gets a channel by its id.
      GetChannel              :: Snowflake -> ChannelRequest Channel
      -- | Edits channels options.
      ModifyChannel           :: ToJSON a  => Snowflake -> a -> ChannelRequest Channel
      -- | Deletes a channel if its id doesn't equal to the id of guild.
      DeleteChannel           :: Snowflake -> ChannelRequest Channel
      -- | Gets a messages from a channel with limit of 100 per request.
      GetChannelMessages      :: Snowflake -> [(Text, Text)] -> ChannelRequest [Message]
      -- | Gets a message in a channel by its id.
      GetChannelMessage       :: Snowflake -> Snowflake -> ChannelRequest Message
      -- | Sends a message to a channel.
      CreateMessage           :: Snowflake -> Text -> ChannelRequest Message
      -- | Sends a message with a file to a channel.
      UploadFile              :: Snowflake -> Text -> ByteString -> ChannelRequest Message
      -- | Edits a message content.
      EditMessage             :: Message   -> Text -> ChannelRequest Message
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
      hashWithSalt s (CreateMessage chan _) = hashWithSalt s ("msg"::Text, chan)
      hashWithSalt s (UploadFile chan _ _)  = hashWithSalt s ("msg"::Text, chan)
      hashWithSalt s (EditMessage (Message _ chan _ _ _ _ _ _ _ _ _ _ _ _) _) =
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

    instance Eq (ChannelRequest a) where
      a == b = hash a == hash b

    instance RateLimit (ChannelRequest a) where
      getRateLimit req = do
        DiscordState {getRateLimits=rl} <- ST.get
        now <- ST.liftIO (fmap round getPOSIXTime :: IO Int)
        ST.liftIO . atomically $ do
          rateLimits <- readTVar rl
          case lookup (hash req) rateLimits of
            Nothing -> return Nothing
            Just a
              | a >= now  -> return $ Just a
              | otherwise -> modifyTVar' rl (Dc.delete $ hash req) >> return Nothing

      setRateLimit req reset = do
        DiscordState {getRateLimits=rl} <- ST.get
        ST.liftIO . atomically . modifyTVar rl $ Dc.insert (hash req) reset

    instance (FromJSON a) => DoFetch (ChannelRequest a) where
      doFetch req = do
        waitRateLimit req
        SyncFetched <$> fetch req

    -- |Sends a request, used by doFetch.
    fetch :: FromJSON a => ChannelRequest a -> DiscordM a
    fetch request = do
      req  <- baseRequest
      (resp, rlRem, rlNext) <- lift $ do
        resp <- case request of
          GetChannel chan -> getWith req
            (baseURL++"/channels/"++chan)

          ModifyChannel chan patch -> customPayloadMethodWith "PATCH" req
            (baseURL++"/channels/"++chan)
            (toJSON patch)

          DeleteChannel chan -> deleteWith req
            (baseURL++"/channels/"++chan)

          GetChannelMessages chan patch -> getWith
            (Prelude.foldr (\(k, v) -> param k .~ [v]) req patch)
            (baseURL++"/channels/"++chan++"/messages")

          GetChannelMessage chan msg -> getWith req
            (baseURL++"/channels/"++chan++"/messages/"++msg)

          CreateMessage chan msg -> postWith req
            (baseURL++"/channels/"++chan++"/messages")
            (object [("content", toJSON msg)])

          UploadFile chan msg file -> postWith
            (req & header "Content-Type" .~ ["multipart/form-data"])
            (baseURL++"/channels/"++chan++"/messages")
            ["content" := msg, "file" := file]

          EditMessage (Message msg chan _ _ _ _ _ _ _ _ _ _ _ _) new ->
            customPayloadMethodWith "PATCH" req
              (baseURL++"/channels/"++chan++"/messages/"++msg)
              (object [("content", toJSON new)])

          DeleteMessage (Message msg chan _ _ _ _ _ _ _ _ _ _ _ _) ->
            deleteWith req
              (baseURL++"/channels/"++chan++"/messages/"++msg)

          BulkDeleteMessage chan msgs -> postWith req
            (baseURL++"/channels/"++chan++"/messages/bulk-delete")
            (object
              [("messages", toJSON
                $ Prelude.map (\(Message msg _ _ _ _ _ _ _ _ _ _ _ _ _) -> msg) msgs)])

          EditChannelPermissions chan perm patch -> putWith req
            (baseURL++"/channels/"++chan++"/permissions/"++perm)
            (toJSON patch)

          GetChannelInvites chan -> getWith req
            (baseURL++"/channels/"++chan++"/invites")

          CreateChannelInvite chan patch -> postWith req
            (baseURL++"/channels/"++chan++"/invites")
            (toJSON patch)

          DeleteChannelPermission chan perm -> deleteWith req
            (baseURL++"/channels/"++chan++"/permissions/"++perm)

          TriggerTypingIndicator chan -> postWith req
            (baseURL++"/channels/"++chan++"/typing")
            (toJSON ([]::[Int]))

          GetPinnedMessages chan -> getWith req
            (baseURL++"/channels/"++chan++"/pins")

          AddPinnedMessage chan msg -> putWith req
            (baseURL++"/channels/"++chan++"/pins/"++msg)
            (toJSON ([]::[Int]))

          DeletePinnedMessage chan msg -> deleteWith req
            (baseURL++"/channels/"++chan++"/pins/"++msg)
        return (justRight . eitherDecode $ resp ^. responseBody
          , justRight . eitherDecodeStrict $ resp ^. responseHeader "X-RateLimit-Remaining"::Int
          , justRight . eitherDecodeStrict $ resp ^. responseHeader "X-RateLimit-Reset"::Int)
      when (rlRem == 0) $ setRateLimit request rlNext
      return resp
