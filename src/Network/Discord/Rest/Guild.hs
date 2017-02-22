{-# LANGUAGE GADTs, OverloadedStrings, InstanceSigs, TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, ScopedTypeVariables #-}
module Network.Discord.Rest.Guild
  (
    GuildRequest(..)
  ) where
    import Data.Maybe
    import Data.Text as T
    import qualified Control.Monad.State as ST (get, put, liftIO)
    import Control.Monad.Morph (lift)
    import Control.Monad (when)

    import Data.Aeson
    import Data.Hashable
    import qualified Network.HTTP.Req as R
    import Data.ByteString.Char8 as B (unpack)
    import Data.ByteString.Lazy as LBS
    import Data.Time.Clock.POSIX

    import Network.Discord.Types as Dc
    import Network.Discord.Rest.Prelude


    -- | Data constructor for Guild requests. See <https://discordapp.com/developers/docs/resources/guild Guild API>
    data GuildRequest a where
      GetGuild                 :: Snowflake -> GuildRequest Guild
      ModifyGuild              :: ToJSON a => Snowflake -> a -> GuildRequest Guild
      DeleteGuild              :: Snowflake -> GuildRequest Guild
      GetGuildChannels         :: Snowflake -> GuildRequest [Channel]
      CreateGuildChannel       :: ToJSON a => Snowflake -> a -> GuildRequest Channel
      ModifyChanPosition       :: ToJSON a => Snowflake -> a -> GuildRequest [Channel]
      GetGuildMember           :: Snowflake -> Snowflake -> GuildRequest Member
      ListGuildMembers         :: Snowflake -> Range -> GuildRequest [Member]
      AddGuildMember           :: ToJSON a => Snowflake -> Snowflake -> a 
                                    -> GuildRequest Member
      ModifyGuildMember        :: ToJSON a => Snowflake -> Snowflake -> a 
                                    -> GuildRequest Member
      RemoveGuildMember        :: Snowflake -> Snowflake -> GuildRequest ()
      GetGuildBans             :: Snowflake -> GuildRequest [User]
      CreateGuildBan           :: Snowflake -> Snowflake -> Integer -> GuildRequest ()
      RemoveGuildBan           :: Snowflake -> Snowflake -> GuildRequest ()
      GetGuildRoles            :: Snowflake -> GuildRequest [Role]
      CreateGuildRole          :: Snowflake -> GuildRequest Role
      ModifyGuildRolePositions :: ToJSON a => Snowflake -> [a] -> GuildRequest [Role]
      ModifyGuildRole          :: ToJSON a => Snowflake -> Snowflake -> a 
                                    -> GuildRequest Role
      DeleteGuildRole          :: Snowflake -> Snowflake -> GuildRequest Role
      GetGuildPruneCount       :: Snowflake -> Integer -> GuildRequest Object
      BeginGuildPrune          :: Snowflake -> Integer -> GuildRequest Object
      GetGuildVoiceRegions     :: Snowflake -> GuildRequest [VoiceRegion]
      GetGuildInvites          :: Snowflake -> GuildRequest [Invite]
      GetGuildIntegrations     :: Snowflake -> GuildRequest [Integration]
      CreateGuildIntegration   :: ToJSON a => Snowflake -> a -> GuildRequest ()
      ModifyGuildIntegration   :: ToJSON a => Snowflake -> Snowflake -> a -> GuildRequest ()
      DeleteGuildIntegration   :: Snowflake -> Snowflake -> GuildRequest ()
      SyncGuildIntegration     :: Snowflake -> Snowflake -> GuildRequest ()
      GetGuildEmbed            :: Snowflake -> GuildRequest GuildEmbed
      ModifyGuildEmbed         :: Snowflake -> GuildEmbed -> GuildRequest GuildEmbed

    -- | Hashable instance to place ChannelRequests in the proper rate limit buckets
    instance Hashable (GuildRequest a) where
      hashWithSalt s (GetGuild g)              = hashWithSalt s ("guild"::Text, g)
      hashWithSalt s (ModifyGuild g _)         = hashWithSalt s ("guild"::Text, g)
      hashWithSalt s (DeleteGuild g)           = hashWithSalt s ("guild"::Text, g)
      hashWithSalt s (GetGuildChannels g)      = hashWithSalt s ("guild_chan"::Text, g)
      hashWithSalt s (CreateGuildChannel g _)  = hashWithSalt s ("guild_chan"::Text, g)
      hashWithSalt s (ModifyChanPosition g _)  = hashWithSalt s ("guild_chan"::Text, g)
      hashWithSalt s (GetGuildMember g _)      = hashWithSalt s ("guild_memb"::Text, g)
      hashWithSalt s (ListGuildMembers g _)  = hashWithSalt s ("guild_membs"::Text, g)
      hashWithSalt s (AddGuildMember g _ _)    = hashWithSalt s ("guild_memb"::Text, g)
      hashWithSalt s (ModifyGuildMember g _ _) = hashWithSalt s ("guild_memb"::Text, g)
      hashWithSalt s (RemoveGuildMember g _)   = hashWithSalt s ("guild_memb"::Text, g)
      hashWithSalt s (GetGuildBans g)          = hashWithSalt s ("guild_bans"::Text, g)
      hashWithSalt s (CreateGuildBan g _ _)    = hashWithSalt s ("guild_ban" ::Text, g)
      hashWithSalt s (RemoveGuildBan g _)      = hashWithSalt s ("guild_ban" ::Text, g)
      hashWithSalt s (GetGuildRoles  g)        = hashWithSalt s ("guild_roles"::Text, g)
      hashWithSalt s (CreateGuildRole g)       = hashWithSalt s ("guild_roles"::Text, g)
      hashWithSalt s (ModifyGuildRolePositions g _)
                                               = hashWithSalt s ("guild_roles"::Text, g)
      hashWithSalt s (ModifyGuildRole g _ _)   = hashWithSalt s ("guild_role" ::Text, g)
      hashWithSalt s (DeleteGuildRole g _ )    = hashWithSalt s ("guild_role" ::Text, g)
      hashWithSalt s (GetGuildPruneCount g _)  = hashWithSalt s ("guild_prune"::Text, g)
      hashWithSalt s (BeginGuildPrune    g _)  = hashWithSalt s ("guild_prune"::Text, g)
      hashWithSalt s (GetGuildVoiceRegions g)  = hashWithSalt s ("guild_voice"::Text, g)
      hashWithSalt s (GetGuildInvites g)       = hashWithSalt s ("guild_invit"::Text, g)
      hashWithSalt s (GetGuildIntegrations g)  = hashWithSalt s ("guild_integ"::Text, g)
      hashWithSalt s (CreateGuildIntegration g _)
                                               = hashWithSalt s ("guild_integ"::Text, g)
      hashWithSalt s (ModifyGuildIntegration g _ _)
                                               = hashWithSalt s ("guild_intgr"::Text, g)
      hashWithSalt s (DeleteGuildIntegration g _)
                                               = hashWithSalt s ("guild_intgr"::Text, g)
      hashWithSalt s (SyncGuildIntegration g _)= hashWithSalt s ("guild_sync" ::Text, g)
      hashWithSalt s (GetGuildEmbed g)         = hashWithSalt s ("guild_embed"::Text, g)
      hashWithSalt s (ModifyGuildEmbed g _)    = hashWithSalt s ("guild_embed"::Text, g)

    instance Eq (GuildRequest a) where
      a == b = hash a == hash b

    instance RateLimit (GuildRequest a) where
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

    instance (FromJSON a) => DoFetch (GuildRequest a) where
      doFetch req = do
        waitRateLimit req
        SyncFetched <$> fetch req

    fetch :: FromJSON a => GuildRequest a -> DiscordM a
    fetch request = do
      opts <- baseRequestOptions
      let makeUrl c = baseUrl R./: "guilds" R./~ (T.pack c)
      let emptyJsonBody = R.ReqBodyJson "" :: R.ReqBodyJson Text
      let get c = (R.req R.GET (makeUrl c) R.NoReqBody R.lbsResponse opts) :: IO R.LbsResponse
      (resp, rlRem, rlNext) <- lift $ do
        resp :: R.LbsResponse <- case request of

          GetGuild chan -> get chan

          ModifyGuild chan patch -> R.req R.PATCH (makeUrl chan) (R.ReqBodyJson patch) R.lbsResponse opts

          DeleteGuild chan -> R.req R.DELETE (makeUrl chan) R.NoReqBody R.lbsResponse opts

          GetGuildChannels chan -> get (chan++"/channels")

          CreateGuildChannel chan patch -> R.req R.POST (makeUrl $ chan++"/channels")
                                                 (R.ReqBodyJson patch) R.lbsResponse opts

          ModifyChanPosition chan patch -> R.req R.PATCH (makeUrl $ chan ++ "/channels")
                                                 (R.ReqBodyJson patch) R.lbsResponse opts

          GetGuildMember chan user -> get (chan++"/members/"++user)

          ListGuildMembers chan range -> get (chan++"/members?limit="++toQueryString range)

          AddGuildMember chan user patch -> R.req R.PUT (makeUrl $ chan++"/members/"++user)
                                                 (R.ReqBodyJson patch) R.lbsResponse opts

          ModifyGuildMember chan user patch ->  R.req R.PATCH (makeUrl $ chan++"/members/"++user)
                                                      (R.ReqBodyJson patch) R.lbsResponse opts

          RemoveGuildMember chan user ->  R.req R.DELETE (makeUrl $ chan++"/members/"++user)
                                                R.NoReqBody R.lbsResponse opts

          GetGuildBans chan -> get (chan++"/bans")

          CreateGuildBan chan user msg -> let payload = object ["delete-message-days" .= msg]
                                          in R.req R.PUT (makeUrl $ chan++"/bans/"++user)
                                                   (R.ReqBodyJson payload) R.lbsResponse opts

          RemoveGuildBan chan user ->  R.req R.DELETE (makeUrl $ chan++"/bans/"++user)
                                             R.NoReqBody R.lbsResponse opts

          GetGuildRoles chan -> get (chan++"/roles")

          CreateGuildRole chan -> R.req R.POST (makeUrl $ chan++"/roles")
                                        emptyJsonBody R.lbsResponse opts

          ModifyGuildRolePositions chan pos -> R.req R.POST (makeUrl $ chan++"/roles")
                                                     (R.ReqBodyJson pos) R.lbsResponse opts

          ModifyGuildRole chan role patch -> R.req R.POST (makeUrl $ chan++"/roles/"++role)
                                                   (R.ReqBodyJson patch) R.lbsResponse opts

          DeleteGuildRole chan role ->  R.req R.DELETE (makeUrl $ chan++"/roles/"++role)
                                              R.NoReqBody R.lbsResponse opts

          GetGuildPruneCount chan days -> get (chan++"/prune?days="++show days)

          BeginGuildPrune chan days -> R.req R.POST (makeUrl $ chan++"/prune?days="++show days)
                                             emptyJsonBody R.lbsResponse opts

          GetGuildVoiceRegions chan -> get (chan++"/regions")

          GetGuildInvites chan -> get (chan++"/invites")

          GetGuildIntegrations chan -> get (chan++"/integrations")

          CreateGuildIntegration chan patch -> R.req R.POST (makeUrl $ chan++"/integrations")
                                                     (R.ReqBodyJson patch) R.lbsResponse opts

          ModifyGuildIntegration chan integ patch ->  R.req R.PATCH (makeUrl $ chan++"/integrations/"++integ)
                                                            (R.ReqBodyJson patch) R.lbsResponse opts

          DeleteGuildIntegration chan integ ->  R.req R.DELETE (makeUrl $ chan++"/integrations/"++integ)
                                                      R.NoReqBody R.lbsResponse opts

          SyncGuildIntegration chan integ -> R.req R.POST (makeUrl $ chan++"/integrations/"++integ)
                                                   emptyJsonBody R.lbsResponse opts

          GetGuildEmbed chan -> get (chan++"/embed")

          ModifyGuildEmbed chan embed -> R.req R.PATCH (makeUrl $ chan++"/embed")
                                               (R.ReqBodyJson embed) R.lbsResponse opts

        let parseIntFrom header = read $ B.unpack $ fromMaybe "0" $ R.responseHeader resp header -- FIXME: default int value
            -- justRight . eitherDecodeStrict $
        return (justRight $ eitherDecode $ (R.responseBody resp :: LBS.ByteString),
                parseIntFrom "X-RateLimit-Remaining", parseIntFrom "X-RateLimit-Reset")
      when (rlRem == 0) $ setRateLimit request rlNext
      return resp
