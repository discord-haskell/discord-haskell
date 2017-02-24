{-# LANGUAGE GADTs, OverloadedStrings, InstanceSigs, TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, ScopedTypeVariables, DataKinds #-}
-- | Provides actions for Channel API interactions
module Network.Discord.Rest.Channel
  (
    ChannelRequest(..)
  ) where

    import Data.Maybe (fromMaybe)

    import Control.Monad (when)

    import Control.Concurrent.STM
    import Control.Monad.Morph (lift)
    import Data.Aeson
    import Data.Hashable

    import Data.Semigroup ((<>))
    import qualified Network.HTTP.Req as R
    import qualified Data.ByteString.Char8 as B (unpack)
    import qualified Data.ByteString.Lazy as LBS
    import Data.Text as T

    import Data.Time.Clock.POSIX
    import qualified Control.Monad.State as ST (get, liftIO)

    import Network.Discord.Rest.Prelude
    import Network.Discord.Types as Dc

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

    -- |Sends a request, used by doFetch.
    fetch :: FromJSON a => ChannelRequest a -> DiscordM a
    fetch request = do
      opts <- baseRequestOptions
      let makeUrl c = baseUrl R./: "channels" R./~ (T.pack c)
      let emptyJsonBody = R.ReqBodyJson "" :: R.ReqBodyJson Text
      let get c = (R.req R.GET (makeUrl c) R.NoReqBody R.lbsResponse opts) :: IO R.LbsResponse
      (resp, rlRem, rlNext) <- lift $ do
        resp :: R.LbsResponse <- case request of

          GetChannel chan -> get $ show chan

          ModifyChannel chan patch ->  R.req R.PATCH (makeUrl $ show chan)
                                             (R.ReqBodyJson patch) R.lbsResponse opts


          DeleteChannel chan ->  R.req R.DELETE (makeUrl $ show chan)
                                       R.NoReqBody R.lbsResponse opts

          GetChannelMessages chan patch -> let opts' :: R.Option 'R.Https = Prelude.foldr (<>) opts (Prelude.map option patch)
                                               option (k,v) = (k R.=: v) :: R.Option 'R.Https
                                           in R.req R.GET (makeUrl $ show chan++"/messages") R.NoReqBody R.lbsResponse opts'

          GetChannelMessage chan msg -> get (show chan++"/messages/"++show msg)

          CreateMessage chan msg em -> let payload = object $ ["content" .= msg] <> maybeEmbed em
                                       in R.req R.POST (makeUrl $ show chan++"/messages")
                                                (R.ReqBodyJson payload) R.lbsResponse opts

          -- TODO: pass json as form, construct proper form-data
          -- https://hackage.haskell.org/package/req-0.2.0/docs/Network-HTTP-Req.html#t:ReqBodyMultipart
          UploadFile chan msg file -> let payload = object ["content" .= msg, "file" .= file]
                                          --mpd = R.header "Content-Type" "multipart/form-data"
                                      in R.req R.POST (makeUrl $ show chan++"/messages")
                                             (R.ReqBodyJson payload) R.lbsResponse opts -- (opts<>mpd)

          EditMessage (Message msg chan _ _ _ _ _ _ _ _ _ _ _ _) new em ->
            let payload = object $ ["content" .= new] <> maybeEmbed em
            in R.req R.PATCH (makeUrl $ show chan++"/messages/"++show msg)
                     (R.ReqBodyJson payload) R.lbsResponse opts


          DeleteMessage (Message msg chan _ _ _ _ _ _ _ _ _ _ _ _) ->
             R.req R.DELETE (makeUrl $ show chan++"/messages/"++show msg) R.NoReqBody R.lbsResponse opts

          BulkDeleteMessage chan msgs -> let payload = object ["messages" .= msgs']
                                             msgs' = Prelude.map (\(Message msg _ _ _ _ _ _ _ _ _ _ _ _ _) -> msg) msgs
                                         in R.req R.POST (makeUrl $ show chan++"/messages/bulk-delete")
                                                  (R.ReqBodyJson payload) R.lbsResponse opts


          EditChannelPermissions chan perm patch -> R.req R.PUT (makeUrl $ show chan++"/permissions/"++show perm)
                                                          (R.ReqBodyJson patch) R.lbsResponse opts

          GetChannelInvites chan -> get (show chan++"/invites")

          CreateChannelInvite chan patch -> R.req R.POST (makeUrl $ show chan++"/invites")
                                                  (R.ReqBodyJson patch) R.lbsResponse opts

          DeleteChannelPermission chan perm ->  R.req R.DELETE (makeUrl $ show chan++"/permissions/"++show perm)
                                                      R.NoReqBody R.lbsResponse opts

          TriggerTypingIndicator chan -> R.req R.POST (makeUrl $ show chan++"/typing")
                                               emptyJsonBody R.lbsResponse opts

          GetPinnedMessages chan -> get (show chan++"/pins")

          AddPinnedMessage chan msg -> R.req R.PUT (makeUrl $ show chan++"/pins/"++show msg)
                                             emptyJsonBody R.lbsResponse opts

          DeletePinnedMessage chan msg ->  R.req R.DELETE (makeUrl $ show chan++"/pins/"++show msg)
                                           R.NoReqBody R.lbsResponse opts

        let parseIntFrom header = read $ B.unpack $ fromMaybe "0" $ R.responseHeader resp header -- FIXME: default int value
            -- justRight . eitherDecodeStrict $
        return (justRight $ eitherDecode $ (R.responseBody resp :: LBS.ByteString),
                parseIntFrom "X-RateLimit-Remaining", parseIntFrom "X-RateLimit-Reset")
      when (rlRem == 0) $ setRateLimit request rlNext
      return resp
      where
        maybeEmbed :: Maybe Embed -> [(Text, Value)]
        maybeEmbed = maybe [] $ \embed -> ["embed" .= embed]
