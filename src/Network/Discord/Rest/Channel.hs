{-# LANGUAGE GADTs, OverloadedStrings, InstanceSigs, TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
module Network.Discord.Rest.Channel
  (
    ChannelRequest(..)
  ) where
    import Data.Text
    import Data.ByteString.Lazy
    import qualified Control.Monad.State as ST (get, put, liftIO)
    import Control.Monad.Morph (lift)
    import Control.Monad (when)

    import Data.Aeson
    import Data.Hashable
    import Network.Wreq
    import Control.Lens
    import Data.Time.Clock.POSIX

    import Network.Discord.Types as Dc
    import Network.Discord.Rest.Prelude

    data ChannelRequest a where
      GetChannel              :: Snowflake -> ChannelRequest Channel
      ModifyChannel           :: ToJSON a => Snowflake -> a -> ChannelRequest Channel
      DeleteChannel           :: Snowflake -> ChannelRequest Channel
      GetChannelMessages      :: Snowflake -> [(Text, Text)] -> ChannelRequest [Message]
      GetChannelMessage       :: Snowflake -> Snowflake -> ChannelRequest Message
      CreateMessage           :: Snowflake -> Text -> ChannelRequest Message
      UploadFile              :: Snowflake -> Text -> ByteString -> ChannelRequest Message
      EditMessage             :: Message   -> Text -> ChannelRequest Message
      DeleteMessage           :: Message   -> ChannelRequest ()
      BulkDeleteMessage       :: Snowflake -> [Message] -> ChannelRequest ()
      EditChannelPermissions  :: Snowflake -> Snowflake -> ChannelRequest ()
      GetChannelInvites       :: Snowflake -> ChannelRequest Object
      CreateChannelInvite     :: Snowflake -> ChannelRequest Object
      DeleteChannelPermission :: Snowflake -> Snowflake -> ChannelRequest ()
      TriggerTypingIndicator  :: Snowflake -> ChannelRequest ()
      GetPinnedMessages       :: Snowflake -> ChannelRequest [Message]
      AddPinnedMessage        :: Snowflake -> Snowflake -> ChannelRequest ()
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
      hashWithSalt s (EditChannelPermissions chan _) = hashWithSalt s ("perms"::Text, chan)
      hashWithSalt s (GetChannelInvites chan) = hashWithSalt s ("invites"::Text, chan)
      hashWithSalt s (CreateChannelInvite chan) = hashWithSalt s ("invites"::Text, chan)
      hashWithSalt s (DeleteChannelPermission chan _) = hashWithSalt s ("perms"::Text, chan)
      hashWithSalt s (TriggerTypingIndicator chan)  = hashWithSalt s ("tti"::Text, chan)
      hashWithSalt s (GetPinnedMessages chan) = hashWithSalt s ("pins"::Text, chan)
      hashWithSalt s (AddPinnedMessage chan _) = hashWithSalt s ("pin"::Text, chan)
      hashWithSalt s (DeletePinnedMessage chan _) = hashWithSalt s ("pin"::Text, chan)

    instance Eq (ChannelRequest a) where
      a == b = hash a == hash b

    instance RateLimit (ChannelRequest a) where
      getRateLimit req = do
        st@DiscordState {getRateLimits=rl} <- ST.get
        now <- ST.liftIO (fmap round getPOSIXTime :: IO Int)
        case lookup (hash req) rl of
          Nothing -> return Nothing
          Just a
            | a >= now  -> return $ Just a
            | otherwise -> ST.put st{getRateLimits=Dc.delete (hash req) rl} >> return Nothing

      setRateLimit req reset = do
        st@DiscordState {getRateLimits=rl} <- ST.get
        ST.put st {getRateLimits=Dc.insert (hash req) reset rl}

    instance (FromJSON a) => DoFetch (ChannelRequest a) where
      doFetch req = do
        waitRateLimit req
        SyncFetched <$> fetch req

    fetch :: FromJSON a => ChannelRequest a -> DiscordM a
    fetch request = do
      req  <- baseRequest
      (resp, rlRem, rlNext) <- lift $ do
        resp <- case request of
          CreateMessage chan msg -> postWith req
            (baseURL ++ "/channels/" ++ chan ++ "/messages")
            (toJSON $ object [("content", toJSON msg)])
          _ -> error "Unsupported call"
        return (justRight . eitherDecode $ resp ^. responseBody
          , justRight . eitherDecodeStrict $ resp ^. responseHeader "X-RateLimit-Remaining"::Int
          , justRight . eitherDecodeStrict $ resp ^. responseHeader "X-RateLimit-Reset"::Int)
      when (rlRem == 0) $ setRateLimit request rlNext
      return resp
