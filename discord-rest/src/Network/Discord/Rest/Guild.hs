{-# LANGUAGE GADTs, OverloadedStrings, InstanceSigs, TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
-- | Provides actions for Guild API interactions.
module Network.Discord.Rest.Guild
  (
    GuildRequest(..)
  ) where
    
    import Data.Aeson
    import Data.Hashable
    import Data.Monoid (mempty)
    import Data.Text as T
    import Network.HTTP.Req ((=:))
    
    import Network.Discord.Rest.Prelude
    import Network.Discord.Types
    import Network.Discord.Rest.HTTP

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
      -- | Create a guild ban, and optionally Delete previous messages sent by the banned
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
      hashWithSalt s (ListGuildMembers g _)    = hashWithSalt s ("guild_membs"::Text, g)
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

    instance (FromJSON a) => DoFetch GuildRequest a where
      doFetch = go
        where
          url = baseUrl /: "guilds"
          go :: DiscordRest m => GuildRequest a -> m a
          go r@(GetGuild guild) = makeRequest r
            $ Get (url // guild) mempty
          go r@(ModifyGuild guild patch) = makeRequest r
            $ Patch (url // guild) (ReqBodyJson patch) mempty
          go r@(DeleteGuild guild) = makeRequest r
            $ Delete (url // guild) mempty
          go r@(GetGuildChannels guild) = makeRequest r
            $ Get (url // guild /: "channels") mempty
          go r@(CreateGuildChannel guild patch) = makeRequest r
            $ Post (url // guild /: "channels") (ReqBodyJson patch) mempty
          go r@(ModifyChanPosition guild patch) = makeRequest r
            $ Post (url // guild /: "channels") (ReqBodyJson patch) mempty
          go r@(GetGuildMember guild member) = makeRequest r
            $ Get (url // guild /: "members" // member) mempty
          go r@(ListGuildMembers guild range) = makeRequest r
            $ Get (url // guild /: "members") (toQueryString range)
          go r@(AddGuildMember guild user patch) = makeRequest r
            $ Put (url // guild /: "members" // user) (ReqBodyJson patch) mempty
          go r@(ModifyGuildMember guild member patch) = makeRequest r
            $ Patch (url // guild /: "members" // member) (ReqBodyJson patch) mempty
          go r@(RemoveGuildMember guild user) = makeRequest r
            $ Delete (url // guild /: "members" // user) mempty
          go r@(GetGuildBans guild) = makeRequest r
            $ Get (url // guild /: "bans") mempty
          go r@(CreateGuildBan guild user msgs) = makeRequest r
            $ Put (url // guild /: "bans" // user)
              (ReqBodyJson $ object [ "delete-message-days" .= msgs])
              mempty
          go r@(RemoveGuildBan guild ban) = makeRequest r
            $ Delete (url // guild /: "bans" // ban) mempty
          go r@(GetGuildRoles guild) = makeRequest r
            $ Get (url // guild /: "roles") mempty
          go r@(CreateGuildRole guild) = makeRequest r
            $ Post (url // guild /: "roles")
              NoReqBody mempty
          go r@(ModifyGuildRolePositions guild patch) = makeRequest r
            $ Post (url // guild /: "roles")
            (ReqBodyJson patch) mempty
          go r@(ModifyGuildRole guild role patch) = makeRequest r
            $ Post (url // guild /: "roles" // role)
              (ReqBodyJson patch) mempty
          go r@(DeleteGuildRole guild role) = makeRequest r
            $ Delete (url // guild /: "roles" // role) mempty
          go r@(GetGuildPruneCount guild days) = makeRequest r
            $ Get (url // guild /: "prune") ("days" =: days)
          go r@(BeginGuildPrune guild days) = makeRequest r
            $ Post (url // guild /: "prune") 
              NoReqBody ("days" =: days)
          go r@(GetGuildVoiceRegions guild) = makeRequest r
            $ Get (url // guild /: "regions") mempty
          go r@(GetGuildInvites guild) = makeRequest r
            $ Get (url // guild /: "invites") mempty
          go r@(GetGuildIntegrations guild) = makeRequest r
            $ Get (url // guild /: "integrations") mempty
          go r@(CreateGuildIntegration guild patch) = makeRequest r
            $ Post (url // guild /: "integrations")
              (ReqBodyJson patch) mempty
          go r@(ModifyGuildIntegration guild integ patch) = makeRequest r
            $ Patch (url // guild /: "integrations" // integ)
              (ReqBodyJson patch) mempty
          go r@(DeleteGuildIntegration guild integ) = makeRequest r
            $ Delete (url // guild /: "integrations" // integ)
              mempty
          go r@(SyncGuildIntegration guild integ) = makeRequest r
            $ Post (url // guild /: "integrations" // integ)
              NoReqBody mempty
          go r@(GetGuildEmbed guild) = makeRequest r
            $ Get (url // guild /: "integrations") mempty
          go r@(ModifyGuildEmbed guild patch) = makeRequest r
            $ Patch (url // guild /: "embed")
              (ReqBodyJson patch) mempty
