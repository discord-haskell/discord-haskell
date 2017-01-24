{-# LANGUAGE GADTs, OverloadedStrings, InstanceSigs, TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
module Network.Discord.Rest.Guild
  (
    GuildRequest(..)
  ) where
    import Data.Text
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


    -- |Data constructor for Guild requests. See <https://discordapp.com/developers/docs/resources/guild Guild API>
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

    -- |Hashable instance to place ChannelRequests in the proper rate limit buckets
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
      req <- baseRequest
      (resp, rlRem, rlNext) <- lift $ do
        resp <- case request of
          GetGuild chan -> getWith req
            (baseURL++"/guilds/"++chan)
          
          ModifyGuild chan patch -> customPayloadMethodWith "PATCH" req
            (baseURL++"/guilds/"++chan)
            (toJSON patch)

          DeleteGuild chan -> deleteWith req
            (baseURL++"/guilds/"++chan)

          GetGuildChannels chan -> getWith req
            (baseURL++"/guilds/"++chan++"/channels")

          CreateGuildChannel chan patch -> postWith req
            (baseURL++"/guilds/"++chan++"/channels")
            (toJSON patch)

          ModifyChanPosition chan patch -> customPayloadMethodWith "PATCH" req
            (baseURL++"/guilds/"++chan++"/channels")
            (toJSON patch)

          GetGuildMember chan user -> getWith req
            (baseURL++"/guilds/"++chan++"/members/"++user)

          ListGuildMembers chan range -> getWith req
            (baseURL++"/guilds/"++chan++"/members?limit="++toQueryString range)

          AddGuildMember chan user patch -> customPayloadMethodWith "PUT" req
            (baseURL++"/guilds/"++chan++"/members/"++user)
            (toJSON patch)

          ModifyGuildMember chan user patch -> customPayloadMethodWith "PATCH" req
            (baseURL++"/guilds/"++chan++"/members/"++user)
            (toJSON patch)

          RemoveGuildMember chan user -> deleteWith req
            (baseURL++"/guilds/"++chan++"/members/"++user)

          GetGuildBans chan -> getWith req
            (baseURL++"/guilds/"++chan++"/bans")

          CreateGuildBan chan user msg -> customPayloadMethodWith "PUT" req
            (baseURL++"/guilds/"++chan++"/bans/"++user)
            ["delete-message-days" := msg]

          RemoveGuildBan chan user -> deleteWith req
            (baseURL++"/guilds/"++chan++"/bans/"++user)

          GetGuildRoles chan -> getWith req
            (baseURL++"/guilds/"++chan++"/roles")

          CreateGuildRole chan -> postWith req
            (baseURL++"/guilds/"++chan++"/roles")
            (toJSON (""::Text))

          ModifyGuildRolePositions chan pos -> postWith req
            (baseURL++"/guilds/"++chan++"/roles")
            (toJSON pos)

          ModifyGuildRole chan role patch -> postWith req
            (baseURL++"/guilds/"++chan++"/roles/"++role)
            (toJSON patch)

          DeleteGuildRole chan role -> deleteWith req
            (baseURL++"/guilds/"++chan++"/roles/"++role)

          GetGuildPruneCount chan days -> getWith req
            (baseURL++"/guilds/"++chan++"/prune?days="++show days)

          BeginGuildPrune chan days -> postWith req
            (baseURL++"/guilds/"++chan++"/prune?days="++show days)
            (toJSON (""::Text))

          GetGuildVoiceRegions chan -> getWith req
            (baseURL++"/guilds/"++chan++"/regions")

          GetGuildInvites chan -> getWith req 
            (baseURL++"/guilds/"++chan++"/invites")

          GetGuildIntegrations chan -> getWith req
            (baseURL++"/guilds/"++chan++"/integrations")

          CreateGuildIntegration chan patch -> postWith req
            (baseURL++"/guilds/"++chan++"/integrations")
            (toJSON patch)

          ModifyGuildIntegration chan integ patch -> customPayloadMethodWith "PATCH" req
            (baseURL++"/guilds/"++chan++"/integrations/"++integ)
            (toJSON patch)

          DeleteGuildIntegration chan integ -> deleteWith req
            (baseURL++"/guilds/"++chan++"/integrations/"++integ)

          SyncGuildIntegration chan integ -> postWith req
            (baseURL++"/guilds/"++chan++"/integrations/"++integ)
            (toJSON (""::Text))

          GetGuildEmbed chan -> getWith req
            (baseURL++"/guilds/"++chan++"/embed")
          
          ModifyGuildEmbed chan embed -> customPayloadMethodWith "PATCH" req
            (baseURL++"/guilds/"++chan++"/embed")
            (toJSON embed)

        return (justRight . eitherDecode $ resp ^. responseBody
          , justRight . eitherDecodeStrict $ resp ^. responseHeader "X-RateLimit-Remaining"::Int
          , justRight . eitherDecodeStrict $ resp ^. responseHeader "X-RateLimit-Reset"::Int)
      when (rlRem == 0) $ setRateLimit request rlNext
      return resp
