{-# LANGUAGE GADTs, OverloadedStrings, InstanceSigs, TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, ScopedTypeVariables  #-}
-- | Provides actions for Guild API interactions.
module Network.Discord.Rest.Guild
  (
    GuildRequest(..)
  ) where
    import Control.Monad (when)
    import Data.Maybe (fromMaybe)

    import Control.Concurrent.STM
    import Control.Monad.Morph (lift)
    import Data.Aeson
    import Data.Hashable

    import qualified Network.HTTP.Req as R
    import Data.ByteString.Char8 as B (unpack)
    import Data.ByteString.Lazy as LBS
    import Data.Text as T
    import Data.Time.Clock.POSIX
    import qualified Control.Monad.State as ST (get, liftIO)

    import Network.Discord.Rest.Prelude
    import Network.Discord.Types as Dc


    -- | Data constructor for Guild requests. See 
    --   <https://discordapp.com/developers/docs/resources/guild Guild API>
    data GuildRequest a where
      -- | Returns the new 'Guild' object for the given id
      GetGuild                 :: Snowflake -> GuildRequest Guild
      -- | Modify a guild's settings. Returns the updated 'Guild' object on success. Fires a
      --   Guild Update 'Event'.
      ModifyGuild              :: ToJSON a => Snowflake -> a -> GuildRequest Guild
      -- | Delete a guild permanently. User must be owner. Fires a Guild Delete 'Event'.
      DeleteGuild              :: Snowflake -> GuildRequest Guild
      -- | Returns a list of guild 'Channel' objects
      GetGuildChannels         :: Snowflake -> GuildRequest [Channel]
      -- | Create a new 'Channel' object for the guild. Requires 'MANAGE_CHANNELS' 
      --   permission. Returns the new 'Channel' object on success. Fires a Channel Create
      --   'Event'
      CreateGuildChannel       :: ToJSON a => Snowflake -> a -> GuildRequest Channel
      -- | Modify the positions of a set of channel objects for the guild. Requires 
      --   'MANAGE_CHANNELS' permission. Returns a list of all of the guild's 'Channel'
      --   objects on success. Fires multiple Channel Update 'Event's.
      ModifyChanPosition       :: ToJSON a => Snowflake -> a -> GuildRequest [Channel]
      -- | Returns a guild 'Member' object for the specified user
      GetGuildMember           :: Snowflake -> Snowflake -> GuildRequest Member
      -- | Returns a list of guild 'Member' objects that are members of the guild.
      ListGuildMembers         :: Snowflake -> Range -> GuildRequest [Member]
      -- | Adds a user to the guild, provided you have a valid oauth2 access token
      --   for the user with the guilds.join scope. Returns the guild 'Member' as the body.
      --   Fires a Guild Member Add 'Event'. Requires the bot to have the 
      --   CREATE_INSTANT_INVITE permission.
      AddGuildMember           :: ToJSON a => Snowflake -> Snowflake -> a 
                                    -> GuildRequest Member
      -- | Modify attributes of a guild 'Member'. Fires a Guild Member Update 'Event'.
      ModifyGuildMember        :: ToJSON a => Snowflake -> Snowflake -> a 
                                    -> GuildRequest ()
      -- | Remove a member from a guild. Requires 'KICK_MEMBER' permission. Fires a
      --   Guild Member Remove 'Event'.
      RemoveGuildMember        :: Snowflake -> Snowflake -> GuildRequest ()
      -- | Returns a list of 'User' objects that are banned from this guild. Requires the
      --   'BAN_MEMBERS' permission
      GetGuildBans             :: Snowflake -> GuildRequest [User]
      -- | Create a guild ban, and optionally delete previous messages sent by the banned
      --   user. Requires the 'BAN_MEMBERS' permission. Fires a Guild Ban Add 'Event'.
      CreateGuildBan           :: Snowflake -> Snowflake -> Integer -> GuildRequest ()
      -- | Remove the ban for a user. Requires the 'BAN_MEMBERS' permissions. 
      --   Fires a Guild Ban Remove 'Event'.
      RemoveGuildBan           :: Snowflake -> Snowflake -> GuildRequest ()
      -- | Returns a list of 'Role' objects for the guild. Requires the 'MANAGE_ROLES'
      --   permission
      GetGuildRoles            :: Snowflake -> GuildRequest [Role]
      -- | Create a new 'Role' for the guild. Requires the 'MANAGE_ROLES' permission.
      --   Returns the new role object on success. Fires a Guild Role Create 'Event'.
      CreateGuildRole          :: Snowflake -> GuildRequest Role
      -- | Modify the positions of a set of role objects for the guild. Requires the 
      --   'MANAGE_ROLES' permission. Returns a list of all of the guild's 'Role' objects
      --   on success. Fires multiple Guild Role Update 'Event's.
      ModifyGuildRolePositions :: ToJSON a => Snowflake -> [a] -> GuildRequest [Role]
      -- | Modify a guild role. Requires the 'MANAGE_ROLES' permission. Returns the 
      --   updated 'Role' on success. Fires a Guild Role Update 'Event's.
      ModifyGuildRole          :: ToJSON a => Snowflake -> Snowflake -> a 
                                    -> GuildRequest Role
      -- | Delete a guild role. Requires the 'MANAGE_ROLES' permission. Fires a Guild Role
      --   Delete 'Event'.
      DeleteGuildRole          :: Snowflake -> Snowflake -> GuildRequest Role
      -- | Returns an object with one 'pruned' key indicating the number of members 
      --   that would be removed in a prune operation. Requires the 'KICK_MEMBERS' 
      --   permission.
      GetGuildPruneCount       :: Snowflake -> Integer -> GuildRequest Object
      -- | Begin a prune operation. Requires the 'KICK_MEMBERS' permission. Returns an
      --   object with one 'pruned' key indicating the number of members that were removed
      --   in the prune operation. Fires multiple Guild Member Remove 'Events'.
      BeginGuildPrune          :: Snowflake -> Integer -> GuildRequest Object
      -- | Returns a list of 'VoiceRegion' objects for the guild. Unlike the similar /voice
      --   route, this returns VIP servers when the guild is VIP-enabled.
      GetGuildVoiceRegions     :: Snowflake -> GuildRequest [VoiceRegion]
      -- | Returns a list of 'Invite' objects for the guild. Requires the 'MANAGE_GUILD'
      --   permission.
      GetGuildInvites          :: Snowflake -> GuildRequest [Invite]
      -- | Return a list of 'Integration' objects for the guild. Requires the 'MANAGE_GUILD'
      --   permission.
      GetGuildIntegrations     :: Snowflake -> GuildRequest [Integration]
      -- | Attach an 'Integration' object from the current user to the guild. Requires the
      --   'MANAGE_GUILD' permission. Fires a Guild Integrations Update 'Event'.
      CreateGuildIntegration   :: ToJSON a => Snowflake -> a -> GuildRequest ()
      -- | Modify the behavior and settings of a 'Integration' object for the guild.
      --   Requires the 'MANAGE_GUILD' permission. Fires a Guild Integrations Update 'Event'.
      ModifyGuildIntegration   :: ToJSON a => Snowflake -> Snowflake -> a -> GuildRequest ()
      -- | Delete the attached 'Integration' object for the guild. Requires the 
      --   'MANAGE_GUILD' permission. Fires a Guild Integrations Update 'Event'.
      DeleteGuildIntegration   :: Snowflake -> Snowflake -> GuildRequest ()
      -- | Sync an 'Integration'. Requires the 'MANAGE_GUILD' permission.
      SyncGuildIntegration     :: Snowflake -> Snowflake -> GuildRequest ()
      -- | Returns the 'GuildEmbed' object. Requires the 'MANAGE_GUILD' permission.
      GetGuildEmbed            :: Snowflake -> GuildRequest GuildEmbed
      -- | Modify a 'GuildEmbed' object for the guild. All attributes may be passed in with
      --   JSON and modified. Requires the 'MANAGE_GUILD' permission. Returns the updated
      --   'GuildEmbed' object.
      ModifyGuildEmbed         :: Snowflake -> GuildEmbed -> GuildRequest GuildEmbed

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

          GetGuild chan -> get $ show chan

          ModifyGuild chan patch -> R.req R.PATCH (makeUrl $ show chan)
                                          (R.ReqBodyJson patch) R.lbsResponse opts

          DeleteGuild chan -> R.req R.DELETE (makeUrl $ show chan)
                                    R.NoReqBody R.lbsResponse opts

          GetGuildChannels chan -> get (show chan++"/channels")

          CreateGuildChannel chan patch -> R.req R.POST (makeUrl $ show chan++"/channels")
                                                 (R.ReqBodyJson patch) R.lbsResponse opts

          ModifyChanPosition chan patch -> R.req R.PATCH (makeUrl $ show chan ++ "/channels")
                                                 (R.ReqBodyJson patch) R.lbsResponse opts

          GetGuildMember chan user -> get (show chan++"/members/"++show user)

          ListGuildMembers chan range -> get (show chan++"/members?limit="++toQueryString range)

          AddGuildMember chan user patch -> R.req R.PUT (makeUrl $ show chan++"/members/"++show user)
                                                 (R.ReqBodyJson patch) R.lbsResponse opts

          ModifyGuildMember chan user patch ->  R.req R.PATCH (makeUrl $ show chan++"/members/"++show user)
                                                      (R.ReqBodyJson patch) R.lbsResponse opts

          RemoveGuildMember chan user ->  R.req R.DELETE (makeUrl $ show chan++"/members/"++show user)
                                                R.NoReqBody R.lbsResponse opts

          GetGuildBans chan -> get (show chan++"/bans")

          CreateGuildBan chan user msg -> let payload = object ["delete-message-days" .= msg]
                                          in R.req R.PUT (makeUrl $ show chan++"/bans/"++show user)
                                                   (R.ReqBodyJson payload) R.lbsResponse opts

          RemoveGuildBan chan user ->  R.req R.DELETE (makeUrl $ show chan++"/bans/"++show user)
                                             R.NoReqBody R.lbsResponse opts

          GetGuildRoles chan -> get (show chan++"/roles")

          CreateGuildRole chan -> R.req R.POST (makeUrl $ show chan++"/roles")
                                        emptyJsonBody R.lbsResponse opts

          ModifyGuildRolePositions chan pos -> R.req R.POST (makeUrl $ show chan++"/roles")
                                                     (R.ReqBodyJson pos) R.lbsResponse opts

          ModifyGuildRole chan role patch -> R.req R.POST (makeUrl $ show chan++"/roles/"++show role)
                                                   (R.ReqBodyJson patch) R.lbsResponse opts

          DeleteGuildRole chan role ->  R.req R.DELETE (makeUrl $ show chan++"/roles/"++show role)
                                              R.NoReqBody R.lbsResponse opts

          GetGuildPruneCount chan days -> get (show chan++"/prune?days="++show days)

          BeginGuildPrune chan days -> R.req R.POST (makeUrl $ show chan++"/prune?days="++show days)
                                             emptyJsonBody R.lbsResponse opts

          GetGuildVoiceRegions chan -> get (show chan++"/regions")

          GetGuildInvites chan -> get (show chan++"/invites")

          GetGuildIntegrations chan -> get (show chan++"/integrations")

          CreateGuildIntegration chan patch -> R.req R.POST (makeUrl $ show chan++"/integrations")
                                                     (R.ReqBodyJson patch) R.lbsResponse opts

          ModifyGuildIntegration chan integ patch ->  R.req R.PATCH (makeUrl $ show chan++"/integrations/"++show integ)
                                                            (R.ReqBodyJson patch) R.lbsResponse opts

          DeleteGuildIntegration chan integ ->  R.req R.DELETE (makeUrl $ show chan++"/integrations/"++show integ)
                                                      R.NoReqBody R.lbsResponse opts

          SyncGuildIntegration chan integ -> R.req R.POST (makeUrl $ show chan++"/integrations/"++show integ)
                                                   emptyJsonBody R.lbsResponse opts

          GetGuildEmbed chan -> get (show chan++"/embed")

          ModifyGuildEmbed chan embed -> R.req R.PATCH (makeUrl $ show chan++"/embed")
                                               (R.ReqBodyJson embed) R.lbsResponse opts

        let parseIntFrom header = read $ B.unpack $ fromMaybe "0" $ R.responseHeader resp header -- FIXME: default int value
            -- justRight . eitherDecodeStrict $
        return (justRight $ eitherDecode $ (R.responseBody resp :: LBS.ByteString),
                parseIntFrom "X-RateLimit-Remaining", parseIntFrom "X-RateLimit-Reset")
      when (rlRem == 0) $ setRateLimit request rlNext
      return resp

