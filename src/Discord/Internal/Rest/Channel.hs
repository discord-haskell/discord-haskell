{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Provides actions for Channel API interactions
module Discord.Internal.Rest.Channel
  ( ChannelRequest(..)
  , MessageDetailedOpts(..)
  , AllowedMentions(..)
  , ReactionTiming(..)
  , MessageTiming(..)
  , ChannelInviteOpts(..)
  , ModifyChannelOpts(..)
  , ChannelPermissionsOpts(..)
  , GroupDMAddRecipientOpts(..)
  , StartThreadOpts(..)
  , StartThreadNoMessageOpts(..)
  , ListThreads(..)
  ) where


import Data.Aeson
import Data.Default (Default, def)
import Data.Emoji (unicodeByName)
import qualified Data.Text as T
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Network.HTTP.Client (RequestBody (RequestBodyBS))
import Network.HTTP.Client.MultipartFormData (partFileRequestBody, partBS)
import Network.HTTP.Req ((/:), (/~))
import qualified Network.HTTP.Req as R

import Discord.Internal.Rest.Prelude
import Discord.Internal.Types
import Control.Monad (join)

instance Request (ChannelRequest a) where
  majorRoute = channelMajorRoute
  jsonRequest = channelJsonRequest

-- | Data constructor for requests. See <https://discord.com/developers/docs/resources/ API>
data ChannelRequest a where
  -- | Gets a channel by its id.
  GetChannel                :: ChannelId -> ChannelRequest Channel
  -- | Edits channels options.
  ModifyChannel             :: ChannelId -> ModifyChannelOpts -> ChannelRequest Channel
  -- | Deletes a channel if its id doesn't equal to the id of guild.
  DeleteChannel             :: ChannelId -> ChannelRequest Channel
  -- | Gets a messages from a channel with limit of 100 per request.
  GetChannelMessages        :: ChannelId -> (Int, MessageTiming) -> ChannelRequest [Message]
  -- | Gets a message in a channel by its id.
  GetChannelMessage         :: (ChannelId, MessageId) -> ChannelRequest Message
  -- | Sends a message to a channel.
  CreateMessage             :: ChannelId -> T.Text -> ChannelRequest Message
  -- | Sends a message with granular controls.
  CreateMessageDetailed     :: ChannelId -> MessageDetailedOpts -> ChannelRequest Message
  -- | Add an emoji reaction to a message. ID must be present for custom emoji
  CreateReaction            :: (ChannelId, MessageId) -> T.Text -> ChannelRequest ()
  -- | Remove a Reaction this bot added
  DeleteOwnReaction         :: (ChannelId, MessageId) -> T.Text -> ChannelRequest ()
  -- | Remove a Reaction someone else added
  DeleteUserReaction        :: (ChannelId, MessageId) -> UserId -> T.Text -> ChannelRequest ()
  -- | Deletes all reactions of a single emoji on a message
  DeleteSingleReaction      :: (ChannelId, MessageId) -> T.Text -> ChannelRequest ()
  -- | List of users that reacted with this emoji
  GetReactions              :: (ChannelId, MessageId) -> T.Text -> (Int, ReactionTiming) -> ChannelRequest [User]
  -- | Delete all reactions on a message
  DeleteAllReactions        :: (ChannelId, MessageId) -> ChannelRequest ()
  -- | Edits a message content.
  EditMessage               :: (ChannelId, MessageId) -> MessageDetailedOpts
                                                      -> ChannelRequest Message
  -- | Deletes a message.
  DeleteMessage             :: (ChannelId, MessageId) -> ChannelRequest ()
  -- | Deletes a group of messages.
  BulkDeleteMessage         :: (ChannelId, [MessageId]) -> ChannelRequest ()
  -- | Edits a permission overrides for a channel.
  EditChannelPermissions    :: ChannelId -> Either RoleId UserId -> ChannelPermissionsOpts -> ChannelRequest ()
  -- | Gets all instant invites to a channel.
  GetChannelInvites         :: ChannelId -> ChannelRequest Object
  -- | Creates an instant invite to a channel.
  CreateChannelInvite       :: ChannelId -> ChannelInviteOpts -> ChannelRequest Invite
  -- | Deletes a permission override from a channel.
  DeleteChannelPermission   :: ChannelId -> Either RoleId UserId -> ChannelRequest ()
  -- | Sends a typing indicator a channel which lasts 10 seconds.
  TriggerTypingIndicator    :: ChannelId -> ChannelRequest ()
  -- | Gets all pinned messages of a channel.
  GetPinnedMessages         :: ChannelId -> ChannelRequest [Message]
  -- | Pins a message.
  AddPinnedMessage          :: (ChannelId, MessageId) -> ChannelRequest ()
  -- | Unpins a message.
  DeletePinnedMessage       :: (ChannelId, MessageId) -> ChannelRequest ()
  -- | Adds a recipient to a Group DM using their access token
  GroupDMAddRecipient       :: ChannelId -> GroupDMAddRecipientOpts -> ChannelRequest ()
  -- | Removes a recipient from a Group DM
  GroupDMRemoveRecipient    :: ChannelId -> UserId -> ChannelRequest ()
  -- | Start a thread from a message
  StartThreadFromMessage    :: ChannelId -> MessageId -> StartThreadOpts -> ChannelRequest Channel
  -- | Start a thread without a message
  StartThreadNoMessage      :: ChannelId -> StartThreadNoMessageOpts -> ChannelRequest Channel
  -- | Join a thread
  JoinThread                :: ChannelId -> ChannelRequest ()
  -- | Add a thread member
  AddThreadMember           :: ChannelId -> UserId -> ChannelRequest ()
  -- | Leave a thread
  LeaveThread               :: ChannelId -> ChannelRequest ()
  -- | Remove a thread member
  RemoveThreadMember        :: ChannelId -> UserId -> ChannelRequest ()
  -- | Get a thread member
  GetThreadMember           :: ChannelId -> UserId -> ChannelRequest ThreadMember
  -- | List the thread members
  ListThreadMembers         :: ChannelId -> ChannelRequest [ThreadMember]
  -- | List public archived threads in the given channel. Optionally before a 
  -- given time, and optional maximum number of threads. Returns the threads, 
  -- thread members,  and whether there are more to collect.
  -- Requires the READ_MESSAGE_HISTORY permission.
  ListPublicArchivedThreads :: ChannelId -> (Maybe UTCTime, Maybe Integer) -> ChannelRequest ListThreads
  -- | List private archived threads in the given channel. Optionally before a 
  -- given time, and optional maximum number of threads. Returns the threads, 
  -- thread members, and whether there are more to collect.
  -- Requires both the READ_MESSAGE_HISTORY and MANAGE_THREADS permissions.
  ListPrivateArchivedThreads :: ChannelId -> (Maybe UTCTime, Maybe Integer) -> ChannelRequest ListThreads
  -- | List joined private archived threads in the given channel. Optionally 
  -- before a  given time, and optional maximum number of threads. Returns the 
  -- threads, thread members, and whether there are more to collect.
  -- Requires both the READ_MESSAGE_HISTORY and MANAGE_THREADS permissions.
  ListJoinedPrivateArchivedThreads :: ChannelId -> (Maybe UTCTime, Maybe Integer) -> ChannelRequest ListThreads


-- | Options for `CreateMessageDetailed` requests.
data MessageDetailedOpts = MessageDetailedOpts
  { -- | The message contents (up to 2000 characters)
    messageDetailedContent                  :: T.Text
  , -- | `True` if this is a TTS message
    messageDetailedTTS                      :: Bool
  , -- | embedded rich content (up to 6000 characters)
    messageDetailedEmbeds                   :: Maybe [CreateEmbed]
  , -- | the contents of the file being sent
    messageDetailedFile                     :: Maybe (T.Text, B.ByteString)
  , -- | allowed mentions for the message
    messageDetailedAllowedMentions          :: Maybe AllowedMentions
  , -- | If `Just`, reply to the message referenced
    messageDetailedReference                :: Maybe MessageReference
  , -- | Message components for the message
    messageDetailedComponents               :: Maybe [ActionRow]
  , -- | IDs of up to 3 `Sticker` in the server to send with the message
    messageDetailedStickerIds               :: Maybe [StickerId]
  } deriving (Show, Read, Eq, Ord)

instance Default MessageDetailedOpts where
  def = MessageDetailedOpts { messageDetailedContent         = ""
                            , messageDetailedTTS             = False
                            , messageDetailedEmbeds          = Nothing
                            , messageDetailedFile            = Nothing
                            , messageDetailedAllowedMentions = Nothing
                            , messageDetailedReference       = Nothing
                            , messageDetailedComponents      = Nothing
                            , messageDetailedStickerIds      = Nothing
                            }

-- | Data constructor for `GetReactions` requests
data ReactionTiming = BeforeReaction MessageId
                    | AfterReaction MessageId
                    | LatestReaction
  deriving (Show, Read, Eq, Ord)

reactionTimingToQuery :: ReactionTiming -> R.Option 'R.Https
reactionTimingToQuery t = case t of
  (BeforeReaction snow) -> "before" R.=: show snow
  (AfterReaction snow) -> "after"  R.=: show snow
  LatestReaction -> mempty

-- | Data constructor for `GetChannelMessages` requests.
-- 
-- See <https://discord.com/developers/docs/resources/channel#get-channel-messages>
data MessageTiming = AroundMessage MessageId
                   | BeforeMessage MessageId
                   | AfterMessage MessageId
                   | LatestMessages
  deriving (Show, Read, Eq, Ord)

messageTimingToQuery :: MessageTiming -> R.Option 'R.Https
messageTimingToQuery t = case t of
  (AroundMessage snow) -> "around" R.=: show snow
  (BeforeMessage snow) -> "before" R.=: show snow
  (AfterMessage snow) -> "after"  R.=: show snow
  LatestMessages -> mempty

-- | Options for `CreateChannelInvite` requests
data ChannelInviteOpts = ChannelInviteOpts
  { -- | How long the invite is valid for (in seconds)
    channelInviteOptsMaxAgeSeconds          :: Maybe Integer
  , -- | How many uses the invite is valid for
    channelInviteOptsMaxUsages              :: Maybe Integer
  , -- | Whether this invite only grants temporary membership
    channelInviteOptsIsTemporary            :: Maybe Bool
  , -- | Don't reuse a similar invite. Useful for creating many unique one time
    -- use invites
    channelInviteOptsDontReuseSimilarInvite :: Maybe Bool
  } deriving (Show, Read, Eq, Ord)

instance ToJSON ChannelInviteOpts where
  toJSON ChannelInviteOpts{..} = objectFromMaybes
                         ["max_age" .=? channelInviteOptsMaxAgeSeconds,
                          "max_uses" .=? channelInviteOptsMaxUsages,
                          "temporary" .=? channelInviteOptsIsTemporary,
                          "unique" .=? channelInviteOptsDontReuseSimilarInvite ]

-- | Options for `ModifyChannel` requests
data ModifyChannelOpts = ModifyChannelOpts
  { -- | (All) The name of the channel (max 100 characters)
    modifyChannelName                 :: Maybe T.Text
  , -- | (All) Position of the channel in the listing
    modifyChannelPosition             :: Maybe Integer
  , -- | (Text) The channel topic text (max 1024 characters)
    modifyChannelTopic                :: Maybe T.Text
  , -- | (Text) Wether the channel is tagged as NSFW
    modifyChannelNSFW                 :: Maybe Bool
  , -- | (Voice) Bitrate (in bps) of a voice channel. Min 8000, max 96000
    -- (128000 for boosted servers)
    modifyChannelBitrate              :: Maybe Integer
  , -- | (Text) The rate limit of the channel, in seconds (0-21600), does not
    -- affect bots and users with @manage_channel@ or @manage_messages@
    -- permissons
    modifyChannelUserRateLimit        :: Maybe Integer
  , -- | (Voice) the user limit of the voice channel, max 99
    modifyChannelUserLimit            :: Maybe Integer
  , -- | (All) The channel permissions
    modifyChannelPermissionOverwrites :: Maybe [Overwrite]
  , -- | (All) The parent category of the channel
    modifyChannelParentId             :: Maybe ChannelId
  , -- | (Text) Auto-archive duration for Threads
    modifyChannelDefaultAutoArchive   :: Maybe Integer
  , -- | (Thread) Whether the thread is archived
    modifyChannelThreadArchived       :: Maybe Bool
  , -- | (Thread) duration in minutes to automatically archive the thread after
    -- recent activity, can be set to: 60, 1440, 4320 or 10080
    modifyChannelThreadAutoArchive    :: Maybe Integer
  , -- | (Thread) Whether the thread is locked. When a thread is locked, only
    -- users with @manage_threads@ can unarchive it
    modifyChannelThreadLocked         :: Maybe Bool
  , -- | (Thread) Whether non-moderators can add other non-moderators to a
    -- thread. Only available on private threads
    modifyChannelThreadInvitable     :: Maybe Bool
  } deriving (Show, Read, Eq, Ord)

instance Default ModifyChannelOpts where
  def = ModifyChannelOpts Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

instance ToJSON ModifyChannelOpts where
  toJSON ModifyChannelOpts{..} = objectFromMaybes
               ["name" .=? modifyChannelName,
                "position" .=? modifyChannelPosition,
                "topic" .=? modifyChannelTopic,
                "nsfw" .=? modifyChannelNSFW,
                "bitrate" .=? modifyChannelBitrate,
                "rate_limit_per_user" .=? modifyChannelUserRateLimit,
                "user_limit" .=? modifyChannelUserLimit,
                "permission_overwrites" .=? modifyChannelPermissionOverwrites,
                "parent_id" .=? modifyChannelParentId,
                "default_auto_archive_duration" .=? modifyChannelDefaultAutoArchive,
                "archived" .=? modifyChannelThreadArchived,
                "auto_archive_duration" .=? modifyChannelThreadAutoArchive,
                "locked" .=? modifyChannelThreadLocked,
                "invitable" .=? modifyChannelThreadInvitable ]

-- | Options for The `EditChannelPermissions` request
--
-- Since the JSON encoding of this datatype will require information in the
-- route (the Either decides whether the overwrite is for a user or a role), we
-- do not provide a ToJSON instance. Instead, the JSON is manually constructed
-- in the 'channelJsonRequest' function.
data ChannelPermissionsOpts = ChannelPermissionsOpts
  { -- | The permission integer for the explicitly allowed permissions
    channelPermissionsOptsAllow :: Integer
  , -- | The permission integer for the explicitly denied permissions
    channelPermissionsOptsDeny :: Integer
  } deriving (Show, Read, Eq, Ord)

-- | Options for `GroupDMAddRecipient` request
--
-- See <https://discord.com/developers/docs/resources/channel#group-dm-add-recipient>
data GroupDMAddRecipientOpts = GroupDMAddRecipientOpts
  { -- | The id of the user to add to the Group DM
    groupDMAddRecipientUserToAdd :: UserId
  , -- | The nickname given to the user being added
    groupDMAddRecipientUserToAddNickName :: T.Text
  , -- | Access token of the user. That user must have granted your app the
    -- @gdm.join@ scope.
    groupDMAddRecipientGDMJoinAccessToken :: T.Text
  } deriving (Show, Read, Eq, Ord)

-- | Options for `StartThreadFromMessage` request
data StartThreadOpts = StartThreadOpts
  { -- | Name of the thread
    startThreadName :: T.Text
  , -- | Period of innactivity after which the thread gets archived in minutes.
    -- 
    -- Can be one of 60, 1440, 4320, 10080
    startThreadAutoArchive :: Maybe Integer
  , -- | Amount of seconds a user has to wait before sending another message
    -- (0-21600)
    startThreadRateLimit :: Maybe Integer
  } deriving (Show, Read, Eq, Ord)

instance ToJSON StartThreadOpts where
  toJSON StartThreadOpts{..} = objectFromMaybes
      [ "name" .== startThreadName
      , "auto_archive_duration" .=? startThreadAutoArchive
      , "rate_limit_per_user" .=? startThreadRateLimit
      ]

-- | Options for `StartThreadNoMessage` request
data StartThreadNoMessageOpts = StartThreadNoMessageOpts
  { -- | Base options for the thread
    startThreadNoMessageBaseOpts :: StartThreadOpts
  , -- | The type of thread to create
    --
    -- Can be @10@, @11@, or @12@. See
    -- <https://discord.com/developers/docs/resources/channel#channel-object-channel-types>
    startThreadNoMessageType :: Integer
  , -- | Whether non-moderators can add other non-moderators to a thread. Only
    -- available when creating a private thread.
    startThreadNoMessageInvitable :: Maybe Bool
  } deriving (Show, Read, Eq, Ord)

instance ToJSON StartThreadNoMessageOpts where
  toJSON StartThreadNoMessageOpts{..} = objectFromMaybes
      [ "name" .== startThreadName startThreadNoMessageBaseOpts
      , "auto_archive_duration" .=? startThreadAutoArchive startThreadNoMessageBaseOpts
      , "rate_limit_per_user" .=? startThreadRateLimit startThreadNoMessageBaseOpts
      , "type" .== startThreadNoMessageType
      , "invitable" .=? startThreadNoMessageInvitable
      ]

-- | Result type of `ListJoinedPrivateArchivedThreads`,
-- `ListPrivateArchivedThreads` and `ListPublicArchivedThreads`
data ListThreads = ListThreads
  { -- | The returned threads
    listThreadsThreads :: [Channel]
  , -- | A thread member object for each returned thread the current user has
    -- joined
    listThreadsMembers :: [ThreadMember]
  ,  -- | Whether there is more data to retrieve
    listThreadsHasMore :: Bool
  } deriving (Show, Read, Eq, Ord)

instance ToJSON ListThreads where
  toJSON ListThreads{..} = object
    [ ("threads", toJSON listThreadsThreads)
    , ("members", toJSON listThreadsMembers)
    , ("has_more", toJSON listThreadsHasMore)
    ]

instance FromJSON ListThreads where
  parseJSON = withObject "ListThreads" $ \o ->
    ListThreads <$> o .: "threads"
                <*> o .: "members"
                <*> o .: "has_more"

channelMajorRoute :: ChannelRequest a -> String
channelMajorRoute c = case c of
  (GetChannel chan) ->                       "get_chan " <> show chan
  (ModifyChannel chan _) ->                  "mod_chan " <> show chan
  (DeleteChannel chan) ->                    "mod_chan " <> show chan
  (GetChannelMessages chan _) ->                  "msg " <> show chan
  (GetChannelMessage (chan, _)) ->            "get_msg " <> show chan
  (CreateMessage chan _) ->                       "msg " <> show chan
  (CreateMessageDetailed chan _) ->               "msg " <> show chan
  (CreateReaction (chan, _) _) ->           "add_react " <> show chan
  (DeleteOwnReaction (chan, _) _) ->            "react " <> show chan
  (DeleteUserReaction (chan, _) _ _) ->         "react " <> show chan
  (DeleteSingleReaction (chan, _) _) ->         "react " <> show chan
  (GetReactions (chan, _) _ _) ->               "react " <> show chan
  (DeleteAllReactions (chan, _)) ->             "react " <> show chan
  (EditMessage (chan, _) _) ->                "get_msg " <> show chan
  (DeleteMessage (chan, _)) ->                "get_msg " <> show chan
  (BulkDeleteMessage (chan, _)) ->           "del_msgs " <> show chan
  (EditChannelPermissions chan _ _) ->          "perms " <> show chan
  (GetChannelInvites chan) ->                 "invites " <> show chan
  (CreateChannelInvite chan _) ->             "invites " <> show chan
  (DeleteChannelPermission chan _) ->           "perms " <> show chan
  (TriggerTypingIndicator chan) ->                "tti " <> show chan
  (GetPinnedMessages chan) ->                    "pins " <> show chan
  (AddPinnedMessage (chan, _)) ->                 "pin " <> show chan
  (DeletePinnedMessage (chan, _)) ->              "pin " <> show chan
  (GroupDMAddRecipient chan _) ->             "groupdm " <> show chan
  (GroupDMRemoveRecipient chan _) ->          "groupdm " <> show chan
  (StartThreadFromMessage chan _ _) ->         "thread " <> show chan
  (StartThreadNoMessage chan _) ->           "thread " <> show chan
  (JoinThread chan) ->                         "thread " <> show chan
  (AddThreadMember chan _) ->                  "thread " <> show chan
  (LeaveThread chan) ->                        "thread " <> show chan
  (RemoveThreadMember chan _) ->               "thread " <> show chan
  (GetThreadMember chan _) ->                  "thread " <> show chan
  (ListThreadMembers chan) ->                  "thread " <> show chan
  (ListPublicArchivedThreads chan _) ->        "thread " <> show chan
  (ListPrivateArchivedThreads chan _) ->       "thread " <> show chan
  (ListJoinedPrivateArchivedThreads chan _) -> "thread " <> show chan

cleanupEmoji :: T.Text -> T.Text
cleanupEmoji emoji =
  let noAngles = T.replace "<" "" (T.replace ">" "" emoji)
      byName = T.pack <$> unicodeByName (T.unpack (T.replace ":" "" emoji))
  in case (byName, T.stripPrefix ":" noAngles) of
    (Just e, _) -> e
    (_, Just a) -> "custom:" <> a
    (_, Nothing) -> noAngles

channels :: R.Url 'R.Https
channels = baseUrl /: "channels"

channelJsonRequest :: ChannelRequest r -> JsonRequest
channelJsonRequest c = case c of
  (GetChannel chan) ->
      Get (channels /~ chan) mempty

  (ModifyChannel chan patch) ->
      Patch (channels /~ chan) (pure (R.ReqBodyJson patch)) mempty

  (DeleteChannel chan) ->
      Delete (channels /~ chan) mempty

  (GetChannelMessages chan (n,timing)) ->
      let n' = max 1 (min 100 n)
          options = "limit" R.=: n' <> messageTimingToQuery timing
      in Get (channels /~ chan /: "messages") options

  (GetChannelMessage (chan, msg)) ->
      Get (channels /~ chan /: "messages" /~ msg) mempty

  (CreateMessage chan msg) ->
      let content = ["content" .= msg]
          body = pure $ R.ReqBodyJson $ object content
      in Post (channels /~ chan /: "messages") body mempty

  (CreateMessageDetailed chan msgOpts) ->
    let fileUpload = messageDetailedFile msgOpts
        filePart =
          ( case fileUpload of
              Nothing -> []
              Just f ->
                [ partFileRequestBody
                    "file"
                    (T.unpack $ fst f)
                    (RequestBodyBS $ snd f)
                ]
          )
            ++ join (maybe [] (maybeEmbed . Just <$>) (messageDetailedEmbeds msgOpts))

        payloadData =  objectFromMaybes $
                        [ "content" .== messageDetailedContent msgOpts
                        , "tts"     .== messageDetailedTTS msgOpts ] ++
                        [ "embeds" .=? ((createEmbed <$>) <$> messageDetailedEmbeds msgOpts)
                        , "allowed_mentions" .=? messageDetailedAllowedMentions msgOpts
                        , "message_reference" .=? messageDetailedReference msgOpts
                        , "components" .=? messageDetailedComponents msgOpts
                        , "sticker_ids" .=? messageDetailedStickerIds msgOpts
                        ]
        payloadPart = partBS "payload_json" $ BL.toStrict $ encode payloadData

        body = R.reqBodyMultipart (payloadPart : filePart)
      in Post (channels /~ chan /: "messages") body mempty

  (CreateReaction (chan, msgid) emoji) ->
      let e = cleanupEmoji emoji
      in Put (channels /~ chan /: "messages" /~ msgid /: "reactions" /: e /: "@me" )
             R.NoReqBody mempty

  (DeleteOwnReaction (chan, msgid) emoji) ->
      let e = cleanupEmoji emoji
      in Delete (channels /~ chan /: "messages" /~ msgid /: "reactions" /: e /: "@me" ) mempty

  (DeleteUserReaction (chan, msgid) uID emoji) ->
      let e = cleanupEmoji emoji
      in Delete (channels /~ chan /: "messages" /~ msgid /: "reactions" /: e /~ uID ) mempty

  (DeleteSingleReaction (chan, msgid) emoji) ->
    let e = cleanupEmoji emoji
    in Delete (channels /~ chan /: "messages" /~ msgid /: "reactions" /: e) mempty

  (GetReactions (chan, msgid) emoji (n, timing)) ->
      let e = cleanupEmoji emoji
          n' = max 1 (min 100 n)
          options = "limit" R.=: n' <> reactionTimingToQuery timing
      in Get (channels /~ chan /: "messages" /~ msgid /: "reactions" /: e) options

  (DeleteAllReactions (chan, msgid)) ->
      Delete (channels /~ chan /: "messages" /~ msgid /: "reactions" ) mempty

  -- copied from CreateMessageDetailed, should be outsourced to function probably
  (EditMessage (chan, msg) msgOpts) ->
    let fileUpload = messageDetailedFile msgOpts
        filePart =
          ( case fileUpload of
              Nothing -> []
              Just f ->
                [ partFileRequestBody
                    "file"
                    (T.unpack $ fst f)
                    (RequestBodyBS $ snd f)
                ]
          )
            ++ join (maybe [] (maybeEmbed . Just <$>) (messageDetailedEmbeds msgOpts))

        payloadData =  objectFromMaybes $
                        [ "content" .== messageDetailedContent msgOpts
                        , "tts"     .== messageDetailedTTS msgOpts ] ++
                        [ "embeds" .=? ((createEmbed <$>) <$> messageDetailedEmbeds msgOpts)
                        , "allowed_mentions" .=? messageDetailedAllowedMentions msgOpts
                        , "message_reference" .=? messageDetailedReference msgOpts
                        , "components" .=? messageDetailedComponents msgOpts
                        , "sticker_ids" .=? messageDetailedStickerIds msgOpts
                        ]
        payloadPart = partBS "payload_json" $ BL.toStrict $ encode payloadData

        body = R.reqBodyMultipart (payloadPart : filePart)
      in Patch (channels /~ chan /: "messages" /~ msg) body mempty

  (DeleteMessage (chan, msg)) ->
      Delete (channels /~ chan /: "messages" /~ msg) mempty

  (BulkDeleteMessage (chan, msgs)) ->
      let body = pure . R.ReqBodyJson $ object ["messages" .= msgs]
      in Post (channels /~ chan /: "messages" /: "bulk-delete") body mempty

  (EditChannelPermissions chan overwriteId (ChannelPermissionsOpts a d)) ->
      let body = R.ReqBodyJson $ object [("type", toJSON (either (const 0) (const 1) overwriteId :: Int))
                                        ,("allow", toJSON a)
                                        ,("deny", toJSON d)]
      in Put (channels /~ chan /: "permissions" /~ either unId unId overwriteId) body mempty

  (GetChannelInvites chan) ->
      Get (channels /~ chan /: "invites") mempty

  (CreateChannelInvite chan patch) ->
      Post (channels /~ chan /: "invites") (pure (R.ReqBodyJson patch)) mempty

  (DeleteChannelPermission chan overwriteId) ->
      Delete (channels /~ chan /: "permissions" /~ either unId unId overwriteId) mempty

  (TriggerTypingIndicator chan) ->
      Post (channels /~ chan /: "typing") (pure R.NoReqBody) mempty

  (GetPinnedMessages chan) ->
      Get (channels /~ chan /: "pins") mempty

  (AddPinnedMessage (chan, msg)) ->
      Put (channels /~ chan /: "pins" /~ msg) R.NoReqBody mempty

  (DeletePinnedMessage (chan, msg)) ->
      Delete (channels /~ chan /: "pins" /~ msg) mempty

  (GroupDMAddRecipient chan (GroupDMAddRecipientOpts uid nick tok)) ->
      Put (channels /~ chan /~ chan /: "recipients" /~ uid)
          (R.ReqBodyJson (object [ ("access_token", toJSON tok)
                                 , ("nick", toJSON nick)]))
          mempty

  (GroupDMRemoveRecipient chan userid) ->
      Delete (channels /~ chan /~ chan /: "recipients" /~ userid) mempty

  (StartThreadFromMessage chan mid sto) ->
      Post (channels /~ chan /: "messages" /~ mid /: "threads")
           (pure $ R.ReqBodyJson $ toJSON sto)
           mempty

  (StartThreadNoMessage chan sto) ->
      Post (channels /~ chan /: "messages" /: "threads")
           (pure $ R.ReqBodyJson $ toJSON sto)
           mempty

  (JoinThread chan) ->
      Put (channels /~ chan /: "thread-members" /: "@me")
          R.NoReqBody mempty

  (AddThreadMember chan uid) ->
      Put (channels /~ chan /: "thread-members" /~ uid)
          R.NoReqBody mempty

  (LeaveThread chan) ->
      Delete (channels /~ chan /: "thread-members" /: "@me")
          mempty

  (RemoveThreadMember chan uid) ->
      Delete (channels /~ chan /: "thread-members" /~ uid)
          mempty

  (GetThreadMember chan uid) ->
      Get (channels /~ chan /: "thread-members" /~ uid)
          mempty

  (ListThreadMembers chan) ->
      Get (channels /~ chan /: "thread-members")
          mempty

  (ListPublicArchivedThreads chan (time, lim)) ->
      Get (channels /~ chan /: "threads" /: "archived" /: "public")
          (maybe mempty ("limit" R.=:) lim <> maybe mempty ("before" R.=:) time)

  (ListPrivateArchivedThreads chan (time, lim)) ->
      Get (channels /~ chan /: "threads" /: "archived" /: "private")
          (maybe mempty ("limit" R.=:) lim <> maybe mempty ("before" R.=:) time)

  (ListJoinedPrivateArchivedThreads chan (time, lim)) ->
      Get (channels /~ chan /: "users" /: "@me" /: "threads" /: "archived" /: "private")
          (maybe mempty ("limit" R.=:) lim <> maybe mempty ("before" R.=:) time)
