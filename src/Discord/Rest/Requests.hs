{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Provides actions for Channel API interactions
module Discord.Rest.Requests
  (
    Request(..)
  , MessageTiming(..)
  , prepareRequest
  , JsonRequest
  ) where


import Data.Aeson
import qualified Data.ByteString.Lazy as BL
import Data.Monoid (mempty, (<>))
import qualified Data.Text as T
import Network.HTTP.Client (RequestBody (RequestBodyLBS))
import Network.HTTP.Client.MultipartFormData (partFileRequestBody)
import Network.HTTP.Req ((/:))
import qualified Network.HTTP.Req as R

import Discord.Rest.Prelude
import Discord.Types

-- | Seriallize request into compontents: api path accessed & request to run
prepareRequest :: Request a -> (String, JsonRequest)
prepareRequest r = (majorRoute r, jsonRequest r)

-- | Data constructor for requests. See <https://discordapp.com/developers/docs/resources/ API>
data Request a where
  -- | Gets a channel by its id.
  GetChannel              :: Snowflake -> Request Channel
  -- | Edits channels options.
  ModifyChannel           :: Snowflake -> ModifyChannelOptions -> Request Channel
  -- | Deletes a channel if its id doesn't equal to the id of guild.
  DeleteChannel           :: Snowflake -> Request Channel
  -- | Gets a messages from a channel with limit of 100 per request.
  GetChannelMessages      :: Snowflake -> (Int, MessageTiming) -> Request [Message]
  -- | Gets a message in a channel by its id.
  GetChannelMessage       :: (Snowflake, Snowflake) -> Request Message
  -- | Sends a message to a channel.
  CreateMessage           :: Snowflake -> T.Text -> Maybe Embed -> Request Message
  -- | Sends a message with a file to a channel.
  UploadFile              :: Snowflake -> FilePath -> BL.ByteString -> Request Message
  -- | Add an emoji reaction to a message. ID must be present for custom emoji
  CreateReaction          :: (Snowflake, Snowflake) -> (T.Text, Maybe Snowflake) -> Request ()
  -- | Remove a Reaction this bot added
  DeleteOwnReaction       :: (Snowflake, Snowflake) -> (T.Text, Maybe Snowflake) -> Request ()
  -- | Remove a Reaction someone else added
  DeleteUserReaction      :: (Snowflake, Snowflake) -> (T.Text, Maybe Snowflake)
                                                    -> Snowflake -> Request ()
  -- | Edits a message content.
  EditMessage             :: (Snowflake, Snowflake) -> T.Text -> Maybe Embed -> Request Message
  -- | Deletes a message.
  DeleteMessage           :: (Snowflake, Snowflake) -> Request ()
  -- | Deletes a group of messages.
  BulkDeleteMessage       :: (Snowflake, [Snowflake]) -> Request ()
  -- | Edits a permission overrides for a channel.
  EditChannelPermissions  :: ToJSON o  => Snowflake -> Snowflake -> o -> Request ()
  -- | Gets all instant invites to a channel.
  GetChannelInvites       :: Snowflake -> Request Object
  -- | Creates an instant invite to a channel.
  CreateChannelInvite     :: ToJSON o  => Snowflake -> o -> Request Object
  -- | Deletes a permission override from a channel.
  DeleteChannelPermission :: Snowflake -> Snowflake -> Request ()
  -- | Sends a typing indicator a channel which lasts 10 seconds.
  TriggerTypingIndicator  :: Snowflake -> Request ()
  -- | Gets all pinned messages of a channel.
  GetPinnedMessages       :: Snowflake -> Request [Message]
  -- | Pins a message.
  AddPinnedMessage        :: Snowflake -> Snowflake -> Request ()
  -- | Unpins a message.
  DeletePinnedMessage     :: Snowflake -> Snowflake -> Request ()

  -- | Returns the new 'Guild' object for the given id
  GetGuild                 :: Snowflake -> Request Guild
  -- | Modify a guild's settings. Returns the updated 'Guild' object on success. Fires a
  --   Guild Update 'Event'.
  ModifyGuild              :: ToJSON o => Snowflake -> o -> Request Guild
  -- | Delete a guild permanently. User must be owner. Fires a Guild Delete 'Event'.
  DeleteGuild              :: Snowflake -> Request Guild
  -- | Returns a list of guild 'Channel' objects
  GetGuildChannels         :: Snowflake -> Request [Channel]
  -- | Create a new 'Channel' object for the guild. Requires 'MANAGE_CHANNELS'
  --   permission. Returns the new 'Channel' object on success. Fires a Channel Create
  --   'Event'
  CreateGuildChannel       :: ToJSON o => Snowflake -> o -> Request Channel
  -- | Modify the positions of a set of channel objects for the guild. Requires
  --   'MANAGE_CHANNELS' permission. Returns a list of all of the guild's 'Channel'
  --   objects on success. Fires multiple Channel Update 'Event's.
  ModifyChanPosition       :: ToJSON o => Snowflake -> o -> Request [Channel]
  -- | Returns a guild 'Member' object for the specified user
  GetGuildMember           :: Snowflake -> Snowflake -> Request GuildMember
  -- | Returns a list of guild 'Member' objects that are members of the guild.
  ListGuildMembers         :: Snowflake -> Range -> Request [GuildMember]
  -- | Adds a user to the guild, provided you have a valid oauth2 access token
  --   for the user with the guilds.join scope. Returns the guild 'Member' as the body.
  --   Fires a Guild Member Add 'Event'. Requires the bot to have the
  --   CREATE_INSTANT_INVITE permission.
  AddGuildMember           :: ToJSON o => Snowflake -> Snowflake -> o
                                -> Request GuildMember
  -- | Modify attributes of a guild 'Member'. Fires a Guild Member Update 'Event'.
  ModifyGuildMember        :: ToJSON o => Snowflake -> Snowflake -> o
                                -> Request ()
  -- | Remove a member from a guild. Requires 'KICK_MEMBER' permission. Fires a
  --   Guild Member Remove 'Event'.
  RemoveGuildMember        :: Snowflake -> Snowflake -> Request ()
  -- | Returns a list of 'User' objects that are banned from this guild. Requires the
  --   'BAN_MEMBERS' permission
  GetGuildBans             :: Snowflake -> Request [User]
  -- | Create a guild ban, and optionally Delete previous messages sent by the banned
  --   user. Requires the 'BAN_MEMBERS' permission. Fires a Guild Ban Add 'Event'.
  CreateGuildBan           :: Snowflake -> Snowflake -> Integer -> Request ()
  -- | Remove the ban for a user. Requires the 'BAN_MEMBERS' permissions.
  --   Fires a Guild Ban Remove 'Event'.
  RemoveGuildBan           :: Snowflake -> Snowflake -> Request ()
  -- | Returns a list of 'Role' objects for the guild. Requires the 'MANAGE_ROLES'
  --   permission
  GetGuildRoles            :: Snowflake -> Request [Role]
  -- | Create a new 'Role' for the guild. Requires the 'MANAGE_ROLES' permission.
  --   Returns the new role object on success. Fires a Guild Role Create 'Event'.
  CreateGuildRole          :: Snowflake -> Request Role
  -- | Modify the positions of a set of role objects for the guild. Requires the
  --   'MANAGE_ROLES' permission. Returns a list of all of the guild's 'Role' objects
  --   on success. Fires multiple Guild Role Update 'Event's.
  ModifyGuildRolePositions :: ToJSON o => Snowflake -> [o] -> Request [Role]
  -- | Modify a guild role. Requires the 'MANAGE_ROLES' permission. Returns the
  --   updated 'Role' on success. Fires a Guild Role Update 'Event's.
  ModifyGuildRole          :: ToJSON o => Snowflake -> Snowflake -> o
                                -> Request Role
  -- | Delete a guild role. Requires the 'MANAGE_ROLES' permission. Fires a Guild Role
  --   Delete 'Event'.
  DeleteGuildRole          :: Snowflake -> Snowflake -> Request Role
  -- | Returns an object with one 'pruned' key indicating the number of members
  --   that would be removed in a prune operation. Requires the 'KICK_MEMBERS'
  --   permission.
  GetGuildPruneCount       :: Snowflake -> Integer -> Request Object
  -- | Begin a prune operation. Requires the 'KICK_MEMBERS' permission. Returns an
  --   object with one 'pruned' key indicating the number of members that were removed
  --   in the prune operation. Fires multiple Guild Member Remove 'Events'.
  BeginGuildPrune          :: Snowflake -> Integer -> Request Object
  -- | Returns a list of 'VoiceRegion' objects for the guild. Unlike the similar /voice
  --   route, this returns VIP servers when the guild is VIP-enabled.
  GetGuildVoiceRegions     :: Snowflake -> Request [VoiceRegion]
  -- | Returns a list of 'Invite' objects for the guild. Requires the 'MANAGE_GUILD'
  --   permission.
  GetGuildInvites          :: Snowflake -> Request [Invite]
  -- | Return a list of 'Integration' objects for the guild. Requires the 'MANAGE_GUILD'
  --   permission.
  GetGuildIntegrations     :: Snowflake -> Request [Integration]
  -- | Attach an 'Integration' object from the current user to the guild. Requires the
  --   'MANAGE_GUILD' permission. Fires a Guild Integrations Update 'Event'.
  CreateGuildIntegration   :: ToJSON o => Snowflake -> o -> Request ()
  -- | Modify the behavior and settings of a 'Integration' object for the guild.
  --   Requires the 'MANAGE_GUILD' permission. Fires a Guild Integrations Update 'Event'.
  ModifyGuildIntegration   :: ToJSON o => Snowflake -> Snowflake -> o -> Request ()
  -- | Delete the attached 'Integration' object for the guild. Requires the
  --   'MANAGE_GUILD' permission. Fires a Guild Integrations Update 'Event'.
  DeleteGuildIntegration   :: Snowflake -> Snowflake -> Request ()
  -- | Sync an 'Integration'. Requires the 'MANAGE_GUILD' permission.
  SyncGuildIntegration     :: Snowflake -> Snowflake -> Request ()
  -- | Returns the 'GuildEmbed' object. Requires the 'MANAGE_GUILD' permission.
  GetGuildEmbed            :: Snowflake -> Request GuildEmbed
  -- | Modify a 'GuildEmbed' object for the guild. All attributes may be passed in with
  --   JSON and modified. Requires the 'MANAGE_GUILD' permission. Returns the updated
  --   'GuildEmbed' object.
  ModifyGuildEmbed         :: Snowflake -> GuildEmbed -> Request GuildEmbed

  -- | Returns the 'User' object of the requester's account. For OAuth2, this requires
  --   the identify scope, which will return the object without an email, and optionally
  --   the email scope, which returns the object with an email.
  GetCurrentUser       :: Request User
  -- | Returns a 'User' for a given user ID
  GetUser              :: Snowflake -> Request User
  -- | Modify the requestors user account settings. Returns a 'User' object on success.
  ModifyCurrentUser    :: ToJSON o => o -> Request User
  -- | Returns a list of user 'Guild' objects the current user is a member of.
  --   Requires the guilds OAuth2 scope.
  GetCurrentUserGuilds :: Range -> Request Guild
  -- | Leave a guild.
  LeaveGuild           :: Snowflake -> Request ()
  -- | Returns a list of DM 'Channel' objects
  GetUserDMs           :: Request [Channel]
  -- | Create a new DM channel with a user. Returns a DM 'Channel' object.
  CreateDM             :: Snowflake -> Request Channel

-- | Data constructor for GetChannelMessages requests. See <https://discordapp.com/developers/docs/resources/channel#get-channel-messages>
data MessageTiming = AroundMessage Snowflake
                   | BeforeMessage Snowflake
                   | AfterMessage Snowflake

messageTimingToQuery :: R.QueryParam p => MessageTiming -> p
messageTimingToQuery t = case t of
  (AroundMessage snow) -> "around" R.=: show snow
  (BeforeMessage snow) -> "before" R.=: show snow
  (AfterMessage snow) -> "after"  R.=: show snow

data ModifyChannelOptions = ModifyChannelOptions
  { modifyName                 :: Maybe String
  , modifyPosition             :: Maybe Integer
  , modifyTopic                :: Maybe String
  , modifyNSFW                 :: Maybe Bool
  , modifyBitrate              :: Maybe Integer
  , modifyUserRateLimit        :: Maybe Integer
  , modifyPermissionOverwrites :: Maybe [Overwrite]
  , modifyParentId             :: Maybe Snowflake
  }

instance ToJSON ModifyChannelOptions where
  toJSON ModifyChannelOptions{..} = object [(name, val) | (name, Just val) <-
               [("name",                   toJSON <$> modifyName),
                ("position",               toJSON <$> modifyPosition),
                ("topic",                  toJSON <$> modifyTopic),
                ("nsfw",                   toJSON <$> modifyNSFW),
                ("bitrate",                toJSON <$> modifyBitrate),
                ("user_limit",             toJSON <$> modifyUserRateLimit),
                ("permission_overwrites",  toJSON <$> modifyPermissionOverwrites),
                ("parent_id",              toJSON <$> modifyParentId) ] ]

majorRoute :: Request a -> String
majorRoute c = case c of
  (GetChannel chan) ->                 "get_chan " <> show chan
  (ModifyChannel chan _) ->            "mod_chan " <> show chan
  (DeleteChannel chan) ->              "mod_chan " <> show chan
  (GetChannelMessages chan _) ->            "msg " <> show chan
  (GetChannelMessage (chan, _)) ->      "get_msg " <> show chan
  (CreateMessage chan _ _) ->               "msg " <> show chan
  (UploadFile chan _ _) ->                  "msg " <> show chan
  (CreateReaction (chan, _) (_, _)) ->     "react" <> show chan
  (DeleteOwnReaction (chan, _) (_, _)) ->  "react" <> show chan
  (DeleteUserReaction (chan, _) (_, _) _) -> "react" <> show chan
  (EditMessage (chan, _) _ _) ->        "get_msg " <> show chan
  (DeleteMessage (chan, _)) ->          "get_msg " <> show chan
  (BulkDeleteMessage (chan, _)) ->     "del_msgs " <> show chan
  (EditChannelPermissions chan _ _) ->    "perms " <> show chan
  (GetChannelInvites chan) ->           "invites " <> show chan
  (CreateChannelInvite chan _) ->       "invites " <> show chan
  (DeleteChannelPermission chan _) ->     "perms " <> show chan
  (TriggerTypingIndicator chan) ->          "tti " <> show chan
  (GetPinnedMessages chan) ->              "pins " <> show chan
  (AddPinnedMessage chan _) ->              "pin " <> show chan
  (DeletePinnedMessage chan _) ->           "pin " <> show chan

  (GetGuild g) ->                         "guild " <> show g
  (ModifyGuild g _) ->                    "guild " <> show g
  (DeleteGuild g) ->                      "guild " <> show g
  (GetGuildChannels g) ->            "guild_chan " <> show g
  (CreateGuildChannel g _) ->        "guild_chan " <> show g
  (ModifyChanPosition g _) ->        "guild_chan " <> show g
  (GetGuildMember g _) ->            "guild_memb " <> show g
  (ListGuildMembers g _) ->         "guild_membs " <> show g
  (AddGuildMember g _ _) ->          "guild_memb " <> show g
  (ModifyGuildMember g _ _) ->       "guild_memb " <> show g
  (RemoveGuildMember g _) ->         "guild_memb " <> show g
  (GetGuildBans g) ->                "guild_bans " <> show g
  (CreateGuildBan g _ _) ->           "guild_ban " <> show g
  (RemoveGuildBan g _) ->             "guild_ban " <> show g
  (GetGuildRoles  g) ->             "guild_roles " <> show g
  (CreateGuildRole g) ->            "guild_roles " <> show g
  (ModifyGuildRolePositions g _) -> "guild_roles " <> show g
  (ModifyGuildRole g _ _) ->         "guild_role " <> show g
  (DeleteGuildRole g _ ) ->          "guild_role " <> show g
  (GetGuildPruneCount g _) ->       "guild_prune " <> show g
  (BeginGuildPrune    g _) ->       "guild_prune " <> show g
  (GetGuildVoiceRegions g) ->       "guild_voice " <> show g
  (GetGuildInvites g) ->            "guild_invit " <> show g
  (GetGuildIntegrations g) ->       "guild_integ " <> show g
  (CreateGuildIntegration g _) ->   "guild_integ " <> show g
  (ModifyGuildIntegration g _ _) -> "guild_intgr " <> show g
  (DeleteGuildIntegration g _) ->   "guild_intgr " <> show g
  (SyncGuildIntegration g _) ->      "guild_sync " <> show g
  (GetGuildEmbed g) ->              "guild_embed " <> show g
  (ModifyGuildEmbed g _) ->         "guild_embed " <> show g

  (GetCurrentUser) ->                        "me "
  (GetUser _) ->                           "user "
  (ModifyCurrentUser _) ->          "modify_user "
  (GetCurrentUserGuilds _) ->   "get_user_guilds "
  (LeaveGuild g) ->                 "leave_guild " <> show g
  (GetUserDMs) ->                       "get_dms "
  (CreateDM _) ->                       "make_dm "



maybeEmbed :: Maybe Embed -> [(T.Text, Value)]
maybeEmbed = maybe [] $ \embed -> ["embed" .= embed]

-- | The base url (Req) for API requests
baseUrl :: R.Url 'R.Https
baseUrl = R.https "discordapp.com" R./: "api" R./: apiVersion
  where apiVersion = "v6"

channels :: R.Url 'R.Https
channels = baseUrl /: "channels"

guilds :: R.Url 'R.Https
guilds = baseUrl /: "guilds"

users :: R.Url 'R.Https
users = baseUrl /: "users"

jsonRequest :: Request r -> JsonRequest
jsonRequest c = case c of
  (GetChannel chan) ->
      Get (channels // chan) mempty
  (ModifyChannel chan patch) ->
      Patch (channels // chan) (R.ReqBodyJson patch) mempty
  (DeleteChannel chan) ->
      Delete (channels // chan) mempty
  (GetChannelMessages chan (n,timing)) ->
      let n' = if n < 1 then 1 else (if n > 100 then 100 else n)
          options = "limit" R.=: n' <> messageTimingToQuery timing
      in Get (channels // chan /: "messages") options
  (GetChannelMessage (chan, msg)) ->
      Get (channels // chan /: "messages" // msg) mempty
  (CreateMessage chan msg embed) ->
      let content = ["content" .= msg] <> maybeEmbed embed
          body = pure $ R.ReqBodyJson $ object content
      in Post (channels // chan /: "messages") body mempty
  (UploadFile chan fileName file) ->
      let part = partFileRequestBody "file" fileName $ RequestBodyLBS file
          body = R.reqBodyMultipart [part]
      in Post (channels // chan /: "messages") body mempty
  (CreateReaction (chan, msgid) (name, rID)) ->
      let emoji = "" <> name <> maybe "" ((<>) ":" . T.pack . show) rID
      in Put (channels // chan /: "messages" // msgid /: "reactions" /: emoji /: "@me" )
             R.NoReqBody mempty
  (DeleteOwnReaction (chan, msgid) (name, rID)) ->
      let emoji = "" <> name <> maybe "" ((<>) ":" . T.pack . show) rID
      in Delete (channels // chan /: "messages" // msgid /: "reactions" /: emoji /: "@me" ) mempty
  (DeleteUserReaction (chan, msgid) (name, rID) uID) ->
      let emoji = "" <> name <> maybe "" ((<>) ":" . T.pack . show) rID
      in Delete (channels // chan /: "messages" // msgid /: "reactions" /: emoji // uID ) mempty
  (EditMessage (chan, msg) new embed) ->
      let content = ["content" .= new] <> maybeEmbed embed
          body = R.ReqBodyJson $ object content
      in Patch (channels // chan /: "messages" // msg) body mempty
  (DeleteMessage (chan, msg)) ->
      Delete (channels // chan /: "messages" // msg) mempty
  (BulkDeleteMessage (chan, msgs)) ->
      let body = pure . R.ReqBodyJson $ object ["messages" .= msgs]
      in Post (channels // chan /: "messages" /: "bulk-delete") body mempty
  (EditChannelPermissions chan perm patch) ->
      Put (channels // chan /: "permissions" // perm) (R.ReqBodyJson patch) mempty
  (GetChannelInvites chan) ->
      Get (channels // chan /: "invites") mempty
  (CreateChannelInvite chan patch) ->
      Post (channels // chan /: "invites") (pure (R.ReqBodyJson patch)) mempty
  (DeleteChannelPermission chan perm) ->
      Delete (channels // chan /: "permissions" // perm) mempty
  (TriggerTypingIndicator chan) ->
      Post (channels // chan /: "typing") (pure R.NoReqBody) mempty
  (GetPinnedMessages chan) ->
      Get (channels // chan /: "pins") mempty
  (AddPinnedMessage chan msg) ->
      Put (channels // chan /: "pins" // msg) R.NoReqBody mempty
  (DeletePinnedMessage chan msg) ->
      Delete (channels // chan /: "pins" // msg) mempty

  (GetGuild guild) ->
      Get (guilds // guild) mempty
  (ModifyGuild guild patch) ->
      Patch (guilds // guild) (R.ReqBodyJson patch) mempty
  (DeleteGuild guild) ->
      Delete (guilds // guild) mempty
  (GetGuildChannels guild) ->
      Get (guilds // guild /: "channels") mempty
  (CreateGuildChannel guild patch) ->
      Post (guilds // guild /: "channels") (pure (R.ReqBodyJson patch)) mempty
  (ModifyChanPosition guild patch) ->
      Post (guilds // guild /: "channels") (pure (R.ReqBodyJson patch)) mempty
  (GetGuildMember guild member) ->
      Get (guilds // guild /: "members" // member) mempty
  (ListGuildMembers guild range) ->
      Get (guilds // guild /: "members") (rangeToOption range)
  (AddGuildMember guild user patch) ->
      Put (guilds // guild /: "members" // user) (R.ReqBodyJson patch) mempty
  (ModifyGuildMember guild member patch) ->
      let body = R.ReqBodyJson patch
      in Patch (guilds // guild /: "members" // member) body mempty
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
  (ModifyGuildRolePositions guild patch) ->
      Post (guilds // guild /: "roles") (pure (R.ReqBodyJson patch)) mempty
  (ModifyGuildRole guild role patch) ->
      Post (guilds // guild /: "roles" // role) (pure (R.ReqBodyJson patch)) mempty
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
  (CreateGuildIntegration guild patch) ->
      Post (guilds // guild /: "integrations") (pure (R.ReqBodyJson patch)) mempty
  (ModifyGuildIntegration guild integ patch) ->
      let body = R.ReqBodyJson patch
      in Patch (guilds // guild /: "integrations" // integ) body mempty
  (DeleteGuildIntegration guild integ) ->
      Delete (guilds // guild /: "integrations" // integ) mempty
  (SyncGuildIntegration guild integ) ->
      Post (guilds // guild /: "integrations" // integ) (pure R.NoReqBody) mempty
  (GetGuildEmbed guild) ->
      Get (guilds // guild /: "integrations") mempty
  (ModifyGuildEmbed guild patch) ->
      Patch (guilds // guild /: "embed") (R.ReqBodyJson patch) mempty


  (GetCurrentUser) -> Get (users /: "@me") mempty

  (GetUser user) -> Get (users // user ) mempty

  (ModifyCurrentUser patch) ->
      Patch (users /: "@me")  (R.ReqBodyJson patch) mempty

  (GetCurrentUserGuilds range) -> Get users $ rangeToOption range

  (LeaveGuild guild) -> Delete (users /: "@me" /: "guilds" // guild) mempty

  (GetUserDMs) -> Get (users /: "@me" /: "channels") mempty

  (CreateDM user) ->
      let body = R.ReqBodyJson $ object ["recipient_id" .= user]
      in Post (users /: "@me" /: "channels") (pure body) mempty
