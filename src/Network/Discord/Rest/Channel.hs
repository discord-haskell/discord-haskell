{-# LANGUAGE GADTs, OverloadedStrings, InstanceSigs, TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, ScopedTypeVariables, DataKinds #-}
module Network.Discord.Rest.Channel
  (
    ChannelRequest(..)
  ) where
    import Data.Maybe (fromMaybe)
    import qualified Data.Text as T
    import qualified Control.Monad.State as ST (get, put, liftIO)
    import Control.Monad.Morph (lift)
    import Control.Monad (when)

    import Data.Aeson
    import Data.Hashable
    import Data.Semigroup ((<>))
    import qualified Network.HTTP.Req as R
    import qualified Data.ByteString.Char8 as B (unpack)
    import qualified Data.ByteString.Lazy as LBS
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
      GetChannelMessages      :: Snowflake -> [(T.Text, T.Text)] -> ChannelRequest [Message]
      -- | Gets a message in a channel by its id.
      GetChannelMessage       :: Snowflake -> Snowflake -> ChannelRequest Message
      -- | Sends a message to a channel.
      CreateMessage           :: Snowflake -> T.Text -> ChannelRequest Message
      -- | Sends a message with a file to a channel.
      UploadFile              :: Snowflake -> T.Text -> FilePath -> ChannelRequest Message
      -- | Edits a message content.
      EditMessage             :: Message   -> T.Text -> ChannelRequest Message
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

    -- | Instance of Hashable that allows to get the major parts of an endpoint.
    instance Hashable (ChannelRequest a) where
      hashWithSalt s (GetChannel chan) = hashWithSalt s ("get_chan"::T.Text, chan)
      hashWithSalt s (ModifyChannel chan _) = hashWithSalt s ("mod_chan"::T.Text, chan)
      hashWithSalt s (DeleteChannel chan) = hashWithSalt s ("mod_chan"::T.Text, chan)
      hashWithSalt s (GetChannelMessages chan _) = hashWithSalt s ("msg"::T.Text, chan)
      hashWithSalt s (GetChannelMessage chan _) = hashWithSalt s ("get_msg"::T.Text, chan)
      hashWithSalt s (CreateMessage chan _) = hashWithSalt s ("msg"::T.Text, chan)
      hashWithSalt s (UploadFile chan _ _)  = hashWithSalt s ("msg"::T.Text, chan)
      hashWithSalt s (EditMessage (Message _ chan _ _ _ _ _ _ _ _ _ _ _ _) _) =
        hashWithSalt s ("get_msg"::T.Text, chan)
      hashWithSalt s (DeleteMessage (Message _ chan _ _ _ _ _ _ _ _ _ _ _ _)) =
        hashWithSalt s ("get_msg"::T.Text, chan)
      hashWithSalt s (BulkDeleteMessage chan _) = hashWithSalt s ("del_msgs"::T.Text, chan)
      hashWithSalt s (EditChannelPermissions chan _ _) = hashWithSalt s ("perms"::T.Text, chan)
      hashWithSalt s (GetChannelInvites chan) = hashWithSalt s ("invites"::T.Text, chan)
      hashWithSalt s (CreateChannelInvite chan _) = hashWithSalt s ("invites"::T.Text, chan)
      hashWithSalt s (DeleteChannelPermission chan _) = hashWithSalt s ("perms"::T.Text, chan)
      hashWithSalt s (TriggerTypingIndicator chan)  = hashWithSalt s ("tti"::T.Text, chan)
      hashWithSalt s (GetPinnedMessages chan) = hashWithSalt s ("pins"::T.Text, chan)
      hashWithSalt s (AddPinnedMessage chan _) = hashWithSalt s ("pin"::T.Text, chan)
      hashWithSalt s (DeletePinnedMessage chan _) = hashWithSalt s ("pin"::T.Text, chan)

    -- |Implementation of equality check that is aware of major parameteres.
    instance Eq (ChannelRequest a) where
      a == b = hash a == hash b

    -- |Implementation of rate limiting logic over all channel procedures.
    instance RateLimit (ChannelRequest a) where
      -- |Gets the delay until an endpoint can be used again.
      getRateLimit req = do
        st@DiscordState {getRateLimits=rl} <- ST.get
        now <- ST.liftIO (fmap round getPOSIXTime :: IO Int)
        case lookup (hash req) rl of
          Nothing -> return Nothing
          Just a
            | a >= now  -> return $ Just a
            | otherwise -> ST.put st{getRateLimits=Dc.delete (hash req) rl} >> return Nothing

      -- |Sets a delay until an endpoint can be used again.
      setRateLimit req reset = do
        st@DiscordState {getRateLimits=rl} <- ST.get
        ST.put st {getRateLimits=Dc.insert (hash req) reset rl}

    -- |Waits for rate limits and then returns fetched request.
    instance (FromJSON a) => DoFetch (ChannelRequest a) where
      doFetch req = do
        waitRateLimit req
        SyncFetched <$> fetch req

    -- |Sends a request, used by doFetch.
    fetch :: FromJSON a => ChannelRequest a -> DiscordM a
    fetch request = do
      opts <- baseRequestOptions
      let makeUrl c = baseUrl R./: "guilds" R./~ (T.pack c)
      let emptyJsonBody = R.ReqBodyJson "" :: R.ReqBodyJson T.Text
      let get c = (R.req R.GET (makeUrl c) R.NoReqBody R.lbsResponse opts) :: IO R.LbsResponse
      (resp, rlRem, rlNext) <- lift $ do
        resp :: R.LbsResponse <- case request of

          GetChannel chan -> get chan

          ModifyChannel chan patch ->  R.req R.PATCH (makeUrl chan)
                                             (R.ReqBodyJson patch) R.lbsResponse opts


          DeleteChannel chan ->  R.req R.DELETE (makeUrl chan) R.NoReqBody R.lbsResponse opts

          GetChannelMessages chan patch -> let opts' :: R.Option 'R.Https = Prelude.foldr (<>) opts (map option patch)
                                               option (k,v) = (k R.=: v) :: R.Option 'R.Https
                                           in R.req R.GET (makeUrl $ chan++"/messages") R.NoReqBody R.lbsResponse opts'

          GetChannelMessage chan msg -> get (chan++"/messages/"++msg)

          CreateMessage chan msg -> let payload = object [("content" .= msg)]
                                    in R.req R.POST (makeUrl $ chan++"/messages")
                                             (R.ReqBodyJson payload) R.lbsResponse opts

          -- TODO: pass json as form, construct proper form-data
          -- https://hackage.haskell.org/package/req-0.2.0/docs/Network-HTTP-Req.html#t:ReqBodyMultipart
          UploadFile chan msg file -> let payload = object ["content" .= msg, "file" .= file]
                                          --mpd = R.header "Content-Type" "multipart/form-data"
                                      in R.req R.POST (makeUrl $ chan++"/messages")
                                             (R.ReqBodyJson payload) R.lbsResponse opts -- (opts<>mpd)

          EditMessage (Message msg chan _ _ _ _ _ _ _ _ _ _ _ _) new ->
            let payload = object ["content" .= new]
            in R.req R.PATCH (makeUrl $ chan++"/messages/"++msg)
                     (R.ReqBodyJson payload) R.lbsResponse opts


          DeleteMessage (Message msg chan _ _ _ _ _ _ _ _ _ _ _ _) ->
             R.req R.DELETE (makeUrl $ chan++"/messages/"++msg) R.NoReqBody R.lbsResponse opts

          BulkDeleteMessage chan msgs -> let payload = object ["messages" .= msgs']
                                             msgs' = Prelude.map (\(Message msg _ _ _ _ _ _ _ _ _ _ _ _ _) -> msg) msgs
                                         in R.req R.POST (makeUrl $ chan++"/messages/bulk-delete")
                                                  (R.ReqBodyJson payload) R.lbsResponse opts


          EditChannelPermissions chan perm patch -> R.req R.PUT (makeUrl $ chan++"/permissions/"++perm)
                                                          (R.ReqBodyJson patch) R.lbsResponse opts

          GetChannelInvites chan -> get (chan++"/invites")

          CreateChannelInvite chan patch -> R.req R.POST (makeUrl $ chan++"/invites") (R.ReqBodyJson patch) R.lbsResponse opts

          DeleteChannelPermission chan perm ->  R.req R.DELETE (makeUrl $ chan++"/permissions/"++perm) R.NoReqBody R.lbsResponse opts

          TriggerTypingIndicator chan -> R.req R.POST (makeUrl $ chan++"/typing") emptyJsonBody R.lbsResponse opts

          GetPinnedMessages chan -> get (chan++"/pins")

          AddPinnedMessage chan msg -> R.req R.PUT (makeUrl $ chan++"/pins/"++msg) emptyJsonBody R.lbsResponse opts

          DeletePinnedMessage chan msg ->  R.req R.DELETE (makeUrl $ chan++"/pins/"++msg) R.NoReqBody R.lbsResponse opts

        let parseIntFrom header = read $ B.unpack $ fromMaybe "0" $ R.responseHeader resp header -- FIXME: default int value
            -- justRight . eitherDecodeStrict $
        return (justRight $ eitherDecode $ (R.responseBody resp :: LBS.ByteString),
                parseIntFrom "X-RateLimit-Remaining", parseIntFrom "X-RateLimit-Reset")
      when (rlRem == 0) $ setRateLimit request rlNext
      return resp
