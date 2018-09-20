{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Provides actions for Channel API interactions
module Discord.Rest.Guild
  ( GuildRequest(..)
  , GuildMembersTiming(..)
  ) where


import Data.Aeson
import Data.Monoid (mempty, (<>))
import Network.HTTP.Req ((/:))
import qualified Network.HTTP.Req as R

import Discord.Rest.Prelude
import Discord.Types

instance Request (GuildRequest a) where
  majorRoute = guildMajorRoute
  jsonRequest = guildJsonRequest

-- | Data constructor for requests. See <https://discordapp.com/developers/docs/resources/ API>
data GuildRequest a where
  -- todo CreateGuild        :: So many parameters
  -- | Returns the new 'Guild' object for the given id
  GetGuild                 :: Snowflake -> GuildRequest Guild
  -- | Modify a guild's settings. Returns the updated 'Guild' object on success. Fires a
  --   Guild Update 'Event'.
  -- todo ModifyGuild              :: ToJSON o => Snowflake -> o -> GuildRequest Guild
  -- | Delete a guild permanently. User must be owner. Fires a Guild Delete 'Event'.
  DeleteGuild              :: Snowflake -> GuildRequest Guild
  -- | Returns a list of guild 'Channel' objects
  GetGuildChannels         :: Snowflake -> GuildRequest [Channel]
  -- | Create a new 'Channel' object for the guild. Requires 'MANAGE_CHANNELS'
  --   permission. Returns the new 'Channel' object on success. Fires a Channel Create
  --   'Event'
  -- todo CreateGuildChannel       :: ToJSON o => Snowflake -> o -> GuildRequest Channel
  -- | Modify the positions of a set of channel objects for the guild. Requires
  --   'MANAGE_CHANNELS' permission. Returns a list of all of the guild's 'Channel'
  --   objects on success. Fires multiple Channel Update 'Event's.
  -- todo ModifyChanPosition       :: ToJSON o => Snowflake -> o -> GuildRequest [Channel]
  -- | Returns a guild 'Member' object for the specified user
  GetGuildMember           :: Snowflake -> Snowflake -> GuildRequest GuildMember
  -- | Returns a list of guild 'Member' objects that are members of the guild.
  ListGuildMembers         :: Snowflake -> GuildMembersTiming -> GuildRequest [GuildMember]
  -- | Adds a user to the guild, provided you have a valid oauth2 access token
  --   for the user with the guilds.join scope. Returns the guild 'Member' as the body.
  --   Fires a Guild Member Add 'Event'. Requires the bot to have the
  --   CREATE_INSTANT_INVITE permission.
  -- todo AddGuildMember           :: ToJSON o => Snowflake -> Snowflake -> o
                                -- -> GuildRequest GuildMember
  -- | Modify attributes of a guild 'Member'. Fires a Guild Member Update 'Event'.
  -- todo ModifyGuildMember        :: ToJSON o => Snowflake -> Snowflake -> o
                                -- -> GuildRequest ()
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
  -- todo ModifyGuildRolePositions :: ToJSON o => Snowflake -> [o] -> GuildRequest [Role]
  -- | Modify a guild role. Requires the 'MANAGE_ROLES' permission. Returns the
  --   updated 'Role' on success. Fires a Guild Role Update 'Event's.
  -- todo ModifyGuildRole          :: ToJSON o => Snowflake -> Snowflake -> o
                                -- -> GuildRequest Role
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
  -- todo CreateGuildIntegration   :: ToJSON o => Snowflake -> o -> GuildRequest ()
  -- | Modify the behavior and settings of a 'Integration' object for the guild.
  --   Requires the 'MANAGE_GUILD' permission. Fires a Guild Integrations Update 'Event'.
  -- todo ModifyGuildIntegration   :: ToJSON o => Snowflake -> Snowflake -> o -> GuildRequest ()
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

data GuildMembersTiming = GuildMembersTiming
                          { guildMembersTimingLimit :: Maybe Int
                          , guildMembersTimingAfter :: Maybe Snowflake
                          }

guildMembersTimingToQuery :: GuildMembersTiming -> R.Option 'R.Https
guildMembersTimingToQuery (GuildMembersTiming mLimit mAfter) =
  let limit = case mLimit of
              Nothing -> mempty
              Just lim -> "limit" R.=: lim
      after = case mAfter of
              Nothing -> mempty
              Just aft -> "after" R.=: show aft
  in limit <> after

guildMajorRoute :: GuildRequest a -> String
guildMajorRoute c = case c of
  (GetGuild g) ->                         "guild " <> show g
  -- (ModifyGuild g _) ->                    "guild " <> show g
  (DeleteGuild g) ->                      "guild " <> show g
  (GetGuildChannels g) ->            "guild_chan " <> show g
  -- (CreateGuildChannel g _) ->        "guild_chan " <> show g
  -- (ModifyChanPosition g _) ->        "guild_chan " <> show g
  (GetGuildMember g _) ->            "guild_memb " <> show g
  (ListGuildMembers g _) ->         "guild_membs " <> show g
  -- (AddGuildMember g _ _) ->          "guild_memb " <> show g
  -- (ModifyGuildMember g _ _) ->       "guild_memb " <> show g
  (RemoveGuildMember g _) ->         "guild_memb " <> show g
  (GetGuildBans g) ->                "guild_bans " <> show g
  (CreateGuildBan g _ _) ->           "guild_ban " <> show g
  (RemoveGuildBan g _) ->             "guild_ban " <> show g
  (GetGuildRoles  g) ->             "guild_roles " <> show g
  (CreateGuildRole g) ->            "guild_roles " <> show g
  -- (ModifyGuildRolePositions g _) -> "guild_roles " <> show g
  -- (ModifyGuildRole g _ _) ->         "guild_role " <> show g
  (DeleteGuildRole g _ ) ->          "guild_role " <> show g
  (GetGuildPruneCount g _) ->       "guild_prune " <> show g
  (BeginGuildPrune    g _) ->       "guild_prune " <> show g
  (GetGuildVoiceRegions g) ->       "guild_voice " <> show g
  (GetGuildInvites g) ->            "guild_invit " <> show g
  (GetGuildIntegrations g) ->       "guild_integ " <> show g
  -- (CreateGuildIntegration g _) ->   "guild_integ " <> show g
  -- (ModifyGuildIntegration g _ _) -> "guild_intgr " <> show g
  (DeleteGuildIntegration g _) ->   "guild_intgr " <> show g
  (SyncGuildIntegration g _) ->      "guild_sync " <> show g
  (GetGuildEmbed g) ->              "guild_embed " <> show g
  (ModifyGuildEmbed g _) ->         "guild_embed " <> show g


-- | The base url (Req) for API requests
baseUrl :: R.Url 'R.Https
baseUrl = R.https "discordapp.com" R./: "api" R./: apiVersion
  where apiVersion = "v6"

guilds :: R.Url 'R.Https
guilds = baseUrl /: "guilds"

guildJsonRequest :: GuildRequest r -> JsonRequest
guildJsonRequest c = case c of
  (GetGuild guild) ->
      Get (guilds // guild) mempty

  -- (ModifyGuild guild patch) ->
      -- Patch (guilds // guild) (R.ReqBodyJson patch) mempty

  (DeleteGuild guild) ->
      Delete (guilds // guild) mempty

  (GetGuildChannels guild) ->
      Get (guilds // guild /: "channels") mempty

  -- (CreateGuildChannel guild patch) ->
      -- Post (guilds // guild /: "channels") (pure (R.ReqBodyJson patch)) mempty

  -- (ModifyChanPosition guild patch) ->
      -- Post (guilds // guild /: "channels") (pure (R.ReqBodyJson patch)) mempty

  (GetGuildMember guild member) ->
      Get (guilds // guild /: "members" // member) mempty

  (ListGuildMembers guild range) ->
      Get (guilds // guild /: "members") (guildMembersTimingToQuery range)

  -- (AddGuildMember guild user patch) ->
      -- Put (guilds // guild /: "members" // user) (R.ReqBodyJson patch) mempty

  -- (ModifyGuildMember guild member patch) ->
      -- let body = R.ReqBodyJson patch
      -- in Patch (guilds // guild /: "members" // member) body mempty

  (RemoveGuildMember guild user) ->
      Delete (guilds // guild /: "members" // user) mempty

  (GetGuildBans guild) ->
      Get (guilds // guild /: "bans") mempty

  (CreateGuildBan guild user msgs) ->
      let body = R.ReqBodyJson (object ["delete-message-days" .= msgs])
      in Put (guilds // guild /: "bans" // user) body mempty

  (RemoveGuildBan guild ban) ->
      Delete (guilds // guild /: "bans" // ban) mempty

  (GetGuildRoles guild) ->
      Get (guilds // guild /: "roles") mempty

  (CreateGuildRole guild) ->
      Post (guilds // guild /: "roles") (pure R.NoReqBody) mempty

  -- (ModifyGuildRolePositions guild patch) ->
      -- Post (guilds // guild /: "roles") (pure (R.ReqBodyJson patch)) mempty
  -- (ModifyGuildRole guild role patch) ->
      -- Post (guilds // guild /: "roles" // role) (pure (R.ReqBodyJson patch)) mempty

  (DeleteGuildRole guild role) ->
      Delete (guilds // guild /: "roles" // role) mempty

  (GetGuildPruneCount guild days) ->
      Get (guilds // guild /: "prune") ("days" R.=: days)

  (BeginGuildPrune guild days) ->
      Post (guilds // guild /: "prune") (pure R.NoReqBody) ("days" R.=: days)

  (GetGuildVoiceRegions guild) ->
      Get (guilds // guild /: "regions") mempty

  (GetGuildInvites guild) ->
      Get (guilds // guild /: "invites") mempty

  (GetGuildIntegrations guild) ->
      Get (guilds // guild /: "integrations") mempty

  -- (CreateGuildIntegration guild patch) ->
      -- Post (guilds // guild /: "integrations") (pure (R.ReqBodyJson patch)) mempty

  -- (ModifyGuildIntegration guild integ patch) ->
      -- let body = R.ReqBodyJson patch
      -- in Patch (guilds // guild /: "integrations" // integ) body mempty

  (DeleteGuildIntegration guild integ) ->
      Delete (guilds // guild /: "integrations" // integ) mempty

  (SyncGuildIntegration guild integ) ->
      Post (guilds // guild /: "integrations" // integ) (pure R.NoReqBody) mempty

  (GetGuildEmbed guild) ->
      Get (guilds // guild /: "integrations") mempty

  (ModifyGuildEmbed guild patch) ->
      Patch (guilds // guild /: "embed") (R.ReqBodyJson patch) mempty

