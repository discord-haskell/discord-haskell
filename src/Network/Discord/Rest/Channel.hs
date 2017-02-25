{-# LANGUAGE GADTs, OverloadedStrings, InstanceSigs, TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
-- | Provides actions for Channel API interactions
module Network.Discord.Rest.Channel
  (
    ChannelRequest(..)
  ) where

    import Control.Concurrent.STM
    import Data.Aeson
    import Data.Hashable

    import Data.Semigroup ((<>))
    import Data.Text as T hiding (map, foldr)

    import Data.Time.Clock.POSIX
    import qualified Control.Monad.State as ST (get, liftIO)

    import Network.Discord.Rest.Prelude
    import Network.Discord.Types as Dc
    import qualified Network.Discord.Rest.HTTP as HTTP

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
      CreateMessage           :: Snowflake -> Text -> Maybe Embed -> ChannelRequest Message
      -- | Sends a message with a file to a channel.
      UploadFile              :: Snowflake -> Text -> FilePath -> ChannelRequest Message
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


    doRequest :: (FromJSON b) => HTTP.Methods -> ChannelRequest b -> IO HTTP.Response
    doRequest (get, HTTP.Post post, HTTP.Put put, HTTP.Patch patch, delete') request = return =<< case request of
          GetChannel chan -> get $ show chan
          ModifyChannel chan patch' ->  patch (show chan) patch'
          DeleteChannel chan ->  delete' (show chan)
          GetChannelMessages chan patch' -> let args = patch' >>= arg
                                                arg (k,v) = (T.unpack k ++ "=" ++ show v) --FIXME: escape
                                           in get (show chan++"/messages?"++args)
          GetChannelMessage chan msg -> get (show chan++"/messages/"++show msg)
          CreateMessage chan msg em -> let payload = object $ ["content" .= msg] <> maybeEmbed em
                                       in post (show chan++"/messages") payload
          -- TODO: pass json as form, construct proper form-data
          -- https://hackage.haskell.org/package/req-0.2.0/docs/Network-HTTP-Req.html#t:ReqBodyMultipart
          UploadFile chan msg file -> let payload = object ["content" .= msg, "file" .= file]
                                          --mpd = R.header "Content-Type" "multipart/form-data"
                                      in post (show chan++"/messages") payload
          EditMessage (Message msg chan _ _ _ _ _ _ _ _ _ _ _ _) new em ->
            let payload = object $ ["content" .= new] <> maybeEmbed em
            in patch (show chan++"/messages/"++show msg) payload
          DeleteMessage (Message msg chan _ _ _ _ _ _ _ _ _ _ _ _) -> delete' (show chan++"/messages/"++show msg)
          BulkDeleteMessage chan msgs -> let payload = object ["messages" .= msgs']
                                             msgs' = Prelude.map (\(Message msg _ _ _ _ _ _ _ _ _ _ _ _ _) -> msg) msgs
                                         in post (show chan++"/messages/bulk-delete") payload
          EditChannelPermissions chan perm patch' -> put (show chan++"/permissions/"++show perm) patch'
          GetChannelInvites chan -> get (show chan++"/invites")
          CreateChannelInvite chan patch' -> post (show chan++"/invites") patch'
          DeleteChannelPermission chan perm ->  delete' (show chan++"/permissions/"++show perm)
          TriggerTypingIndicator chan -> post (show chan++"/typing") noPayload
          GetPinnedMessages chan -> get (show chan++"/pins")
          AddPinnedMessage chan msg -> put (show chan++"/pins/"++show msg) noPayload
          DeletePinnedMessage chan msg ->  delete' (show chan++"/pins/"++show msg)

      where
        maybeEmbed :: Maybe Embed -> [(Text, Value)]
        maybeEmbed = maybe [] $ \embed -> ["embed" .= embed]
        noPayload = []::[Int]

    -- |Sends a request, used by doFetch.
    fetch :: (FromJSON b) => ChannelRequest b -> DiscordM b
    fetch = HTTP.fetch HTTP.Channel doRequest

