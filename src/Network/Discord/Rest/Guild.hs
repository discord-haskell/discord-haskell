{-# LANGUAGE GADTs, OverloadedStrings, InstanceSigs, TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
module Network.Discord.Rest.Guild
  (
    GuildRequest(..)
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

    data GuildRequest a where
      GetGuild    :: Snowflake -> GuildRequest Guild
      ModifyGuild :: ToJSON a => Snowflake -> a -> GuildRequest Guild
      DeleteGuild :: Snowflake -> GuildRequest Guild
      GetGuildChannels   :: Snowflake -> GuildRequest [Channel]
      CreateGuildChannel :: ToJSON a => Snowflake -> a -> GuildRequest Channel
      ModifyChanPosition :: ToJSON a => Snowflake -> a -> GuildRequest [Channel]
      GetGuildMember     :: Snowflake -> Snowflake -> GuildRequest Member
      ListGuildMembers   :: Snowflake -> Integer -> Integer -> GuildRequest [Member]
      AddGuildMember     :: ToJSON a => Snowflake -> Snowflake -> a -> GuildRequest Member
      ModifyGuildMember  :: ToJSON a => Snowflake -> Snowflake -> a -> GuildRequest Member
      RemoveGuildMember  :: Snowflake -> Snowflake -> GuildRequest ()
      GetGuildBans       :: Snowflake -> GuildRequest [User]
      CreateGuildBan     :: Snowflake -> Snowflake -> Integer -> GuildRequest ()
      RemoveGuildBan     :: Snowflake -> Snowflake -> GuildRequest ()
      GetGuildRoles      :: Snowlfake -> GuildRequest [Role]
      CreateGuildRole    :: Snowflake -> GuildRequest Role
      ModifyGuildRolePositions :: ToJSON a => Snowflake -> [a] -> GuildRequest [Role]
      ModifyGuildRole    :: ToJSON a => Snowflake -> Snowflake -> a -> GuildRequest Role
      DeleteGuildRole    :: Snowflake -> Snowflake -> GuildRequest Role
      GetGuildPruneCount :: Snowflake -> Integer -> GuildRequest Object
      BeginGuildPrune    :: Snowflake -> Integer -> GuildRequest Object
      GetGuildVoiceRegions :: Snowflake -> GuildRequest [VoiceRegion]
      GetGuildInvites    :: Snowflake -> GuildRequest [Invite]
      GetGuildIntegrations :: Snowflake -> GuildRequest [Integration]
      CreateGuildIntegration :: ToJSON a => Snowflake -> a -> GuildRequest ()
      ModifyGuildIntegration :: Snowflake -> Snowflake -> GuildRequest ()
      DeleteGuildIntegration :: Snowflake -> Snowflake -> GuildRequest ()
      SyncGuildIntegration   :: Snowflake -> Snowflake -> GuildRequest ()
      GetGuildEmbed          :: Snowflake -> GuildRequest GuildEmbed
      ModifyGuildEmbed       :: Snowflake -> GuildEmbed -> GuildRequest GuildEmbed

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
