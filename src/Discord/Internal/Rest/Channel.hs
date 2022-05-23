{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE InstanceSigs #-}
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
import Network.HTTP.Req ((/:))
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


-- | Data constructor for CreateMessageDetailed requests.
data MessageDetailedOpts = MessageDetailedOpts
  { messageDetailedContent                  :: T.Text
  , messageDetailedTTS                      :: Bool
  , messageDetailedEmbeds                   :: Maybe [CreateEmbed]
  , messageDetailedFile                     :: Maybe (T.Text, B.ByteString)
  , messageDetailedAllowedMentions          :: Maybe AllowedMentions
  , messageDetailedReference                :: Maybe MessageReference
  , messageDetailedComponents               :: Maybe [ActionRow]
  , messageDetailedStickerIds               :: Maybe [StickerId]
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

-- | Data constructor for GetReaction requests
data ReactionTiming = BeforeReaction MessageId
                    | AfterReaction MessageId
                    | LatestReaction
  deriving (Show, Read, Eq, Ord)

reactionTimingToQuery :: ReactionTiming -> R.Option 'R.Https
reactionTimingToQuery t = case t of
  (BeforeReaction snow) -> "before" R.=: show snow
  (AfterReaction snow) -> "after"  R.=: show snow
  (LatestReaction) -> mempty

-- | Data constructor for GetChannelMessages requests. See <https://discord.com/developers/docs/resources/channel#get-channel-messages>
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
  (LatestMessages) -> mempty

data ChannelInviteOpts = ChannelInviteOpts
  { channelInviteOptsMaxAgeSeconds          :: Maybe Integer
  , channelInviteOptsMaxUsages              :: Maybe Integer
  , channelInviteOptsIsTemporary            :: Maybe Bool
  , channelInviteOptsDontReuseSimilarInvite :: Maybe Bool
  } deriving (Show, Read, Eq, Ord)

instance ToJSON ChannelInviteOpts where
  toJSON ChannelInviteOpts{..} = object [(name, val) | (name, Just val) <-
                         [("max_age",   toJSON <$> channelInviteOptsMaxAgeSeconds),
                          ("max_uses",  toJSON <$> channelInviteOptsMaxUsages),
                          ("temporary", toJSON <$> channelInviteOptsIsTemporary),
                          ("unique",    toJSON <$> channelInviteOptsDontReuseSimilarInvite) ] ]

data ModifyChannelOpts = ModifyChannelOpts
  { modifyChannelName                 :: Maybe T.Text
  , modifyChannelPosition             :: Maybe Integer
  , modifyChannelTopic                :: Maybe T.Text
  , modifyChannelNSFW                 :: Maybe Bool
  , modifyChannelBitrate              :: Maybe Integer
  , modifyChannelUserRateLimit        :: Maybe Integer
  , modifyChannelPermissionOverwrites :: Maybe [Overwrite]
  , modifyChannelParentId             :: Maybe ChannelId
  , modifyChannelDefaultAutoArchive   :: Maybe Integer
  , modifyChannelThreadArchived       :: Maybe Bool
  , modifyChannelThreadAutoArchive    :: Maybe Integer
  , modifyChannelThreadLocked         :: Maybe Bool
  , modifyChannelThreadInvitiable     :: Maybe Bool
  } deriving (Show, Read, Eq, Ord)

instance Default ModifyChannelOpts where
  def = ModifyChannelOpts Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

instance ToJSON ModifyChannelOpts where
  toJSON ModifyChannelOpts{..} = object [(name, val) | (name, Just val) <-
               [("name",       toJSON <$> modifyChannelName),
                ("position",   toJSON <$> modifyChannelPosition),
                ("topic",      toJSON <$> modifyChannelTopic),
                ("nsfw",       toJSON <$> modifyChannelNSFW),
                ("bitrate",    toJSON <$> modifyChannelBitrate),
                ("user_limit", toJSON <$> modifyChannelUserRateLimit),
                ("permission_overwrites",  toJSON <$> modifyChannelPermissionOverwrites),
                ("parent_id",  toJSON <$> modifyChannelParentId),
                ("default_auto_archive_duration",  toJSON <$> modifyChannelDefaultAutoArchive),
                ("archived",  toJSON <$> modifyChannelThreadArchived),
                ("auto_archive_duration",  toJSON <$> modifyChannelThreadAutoArchive),
                ("locked",  toJSON <$> modifyChannelThreadLocked),
                ("invitable",  toJSON <$> modifyChannelThreadInvitiable) ] ]

-- | Since the JSON encoding of this datatype will require information in the
-- route (the Either decides whether the overwrite is for a user or a role), we
-- do not provide a ToJSON instance. Instead, the JSON is manually constructed
-- in the 'channelJsonRequest' function.
data ChannelPermissionsOpts = ChannelPermissionsOpts
  { channelPermissionsOptsAllow :: Integer
  , channelPermissionsOptsDeny :: Integer
  } deriving (Show, Read, Eq, Ord)

-- | https://discord.com/developers/docs/resources/channel#group-dm-add-recipient
data GroupDMAddRecipientOpts = GroupDMAddRecipientOpts
  { groupDMAddRecipientUserToAdd :: UserId
  , groupDMAddRecipientUserToAddNickName :: T.Text
  , groupDMAddRecipientGDMJoinAccessToken :: T.Text
  } deriving (Show, Read, Eq, Ord)

data StartThreadOpts = StartThreadOpts 
  { startThreadName :: T.Text
  , startThreadAutoArchive :: Maybe Integer -- ^ can be one of 60, 1440, 4320, 10080
  , startThreadRateLimit :: Maybe Integer
  } deriving (Show, Read, Eq, Ord)

instance ToJSON StartThreadOpts where
  toJSON StartThreadOpts{..} = object [ (name, value) | (name, Just value) <- 
      [ ("name", toJSON <$> pure startThreadName)
      , ("auto_archive_duration", toJSON <$> startThreadAutoArchive)
      , ("rate_limit_per_user", toJSON <$> startThreadRateLimit)
      ]
    ]

data StartThreadNoMessageOpts = StartThreadNoMessageOpts
  { startThreadNoMessageBaseOpts :: StartThreadOpts
  , startThreadNoMessageType :: Integer -- ^ 10, 11, or 12 (https://discord.com/developers/docs/resources/channel#channel-object-channel-types)
  , startThreadNoMessageInvitable :: Maybe Bool
  } deriving (Show, Read, Eq, Ord)

instance ToJSON StartThreadNoMessageOpts where
  toJSON StartThreadNoMessageOpts{..} = object [ (name, value) | (name, Just value) <- 
      [ ("name", toJSON <$> pure (startThreadName startThreadNoMessageBaseOpts))
      , ("auto_archive_duration", toJSON <$> (startThreadAutoArchive startThreadNoMessageBaseOpts))
      , ("rate_limit_per_user", toJSON <$> (startThreadRateLimit startThreadNoMessageBaseOpts))
      , ("type", toJSON <$> pure startThreadNoMessageType)
      , ("invitable", toJSON <$> startThreadNoMessageInvitable)
      ]
    ]

data ListThreads = ListThreads 
  { listThreadsThreads :: [Channel]
  , listThreadsMembers :: [ThreadMember]
  , listThreadsHasMore :: Bool -- ^ whether there is more data to retrieve
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
      Get (channels // chan) mempty

  (ModifyChannel chan patch) ->
      Patch (channels // chan) (pure (R.ReqBodyJson patch)) mempty

  (DeleteChannel chan) ->
      Delete (channels // chan) mempty

  (GetChannelMessages chan (n,timing)) ->
      let n' = max 1 (min 100 n)
          options = "limit" R.=: n' <> messageTimingToQuery timing
      in Get (channels // chan /: "messages") options

  (GetChannelMessage (chan, msg)) ->
      Get (channels // chan /: "messages" // msg) mempty

  (CreateMessage chan msg) ->
      let content = ["content" .= msg]
          body = pure $ R.ReqBodyJson $ object content
      in Post (channels // chan /: "messages") body mempty

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

        payloadData =  object $ [ "content" .= messageDetailedContent msgOpts
                                , "tts"     .= messageDetailedTTS msgOpts ] ++
                                [ name .= value | (name, Just value) <-
                                  [ ("embeds", toJSON . (createEmbed <$>) <$> messageDetailedEmbeds msgOpts)
                                  , ("allowed_mentions", toJSON <$> messageDetailedAllowedMentions msgOpts)
                                  , ("message_reference", toJSON <$> messageDetailedReference msgOpts)
                                  , ("components", toJSON <$> messageDetailedComponents msgOpts)
                                  , ("sticker_ids", toJSON <$> messageDetailedStickerIds msgOpts)
                                  ] ]
        payloadPart = partBS "payload_json" $ BL.toStrict $ encode payloadData

        body = R.reqBodyMultipart (payloadPart : filePart)
      in Post (channels // chan /: "messages") body mempty

  (CreateReaction (chan, msgid) emoji) ->
      let e = cleanupEmoji emoji
      in Put (channels // chan /: "messages" // msgid /: "reactions" /: e /: "@me" )
             R.NoReqBody mempty

  (DeleteOwnReaction (chan, msgid) emoji) ->
      let e = cleanupEmoji emoji
      in Delete (channels // chan /: "messages" // msgid /: "reactions" /: e /: "@me" ) mempty

  (DeleteUserReaction (chan, msgid) uID emoji) ->
      let e = cleanupEmoji emoji
      in Delete (channels // chan /: "messages" // msgid /: "reactions" /: e // uID ) mempty

  (DeleteSingleReaction (chan, msgid) emoji) ->
    let e = cleanupEmoji emoji
    in Delete (channels // chan /: "messages" // msgid /: "reactions" /: e) mempty

  (GetReactions (chan, msgid) emoji (n, timing)) ->
      let e = cleanupEmoji emoji
          n' = max 1 (min 100 n)
          options = "limit" R.=: n' <> reactionTimingToQuery timing
      in Get (channels // chan /: "messages" // msgid /: "reactions" /: e) options

  (DeleteAllReactions (chan, msgid)) ->
      Delete (channels // chan /: "messages" // msgid /: "reactions" ) mempty

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

        payloadData =  object $ [ "content" .= messageDetailedContent msgOpts
                                , "tts"     .= messageDetailedTTS msgOpts ] ++
                                [ name .= value | (name, Just value) <-
                                  [ ("embeds", toJSON . (createEmbed <$>) <$> messageDetailedEmbeds msgOpts)
                                  , ("allowed_mentions", toJSON <$> messageDetailedAllowedMentions msgOpts)
                                  , ("message_reference", toJSON <$> messageDetailedReference msgOpts)
                                  , ("components", toJSON <$> messageDetailedComponents msgOpts)
                                  , ("sticker_ids", toJSON <$> messageDetailedStickerIds msgOpts)
                                  ] ]
        payloadPart = partBS "payload_json" $ BL.toStrict $ encode payloadData

        body = R.reqBodyMultipart (payloadPart : filePart)
      in Patch (channels // chan /: "messages" // msg) body mempty

  (DeleteMessage (chan, msg)) ->
      Delete (channels // chan /: "messages" // msg) mempty

  (BulkDeleteMessage (chan, msgs)) ->
      let body = pure . R.ReqBodyJson $ object ["messages" .= msgs]
      in Post (channels // chan /: "messages" /: "bulk-delete") body mempty

  (EditChannelPermissions chan overwriteId (ChannelPermissionsOpts a d)) ->
      let body = R.ReqBodyJson $ object [("type", toJSON $ (either (const 0) (const 1) overwriteId :: Int))
                                        ,("allow", toJSON a)
                                        ,("deny", toJSON d)]
      in Put (channels // chan /: "permissions" // either unId unId overwriteId) body mempty

  (GetChannelInvites chan) ->
      Get (channels // chan /: "invites") mempty

  (CreateChannelInvite chan patch) ->
      Post (channels // chan /: "invites") (pure (R.ReqBodyJson patch)) mempty

  (DeleteChannelPermission chan overwriteId) ->
      Delete (channels // chan /: "permissions" // either unId unId overwriteId) mempty

  (TriggerTypingIndicator chan) ->
      Post (channels // chan /: "typing") (pure R.NoReqBody) mempty

  (GetPinnedMessages chan) ->
      Get (channels // chan /: "pins") mempty

  (AddPinnedMessage (chan, msg)) ->
      Put (channels // chan /: "pins" // msg) R.NoReqBody mempty

  (DeletePinnedMessage (chan, msg)) ->
      Delete (channels // chan /: "pins" // msg) mempty

  (GroupDMAddRecipient chan (GroupDMAddRecipientOpts uid nick tok)) ->
      Put (channels // chan // chan /: "recipients" // uid)
          (R.ReqBodyJson (object [ ("access_token", toJSON tok)
                                 , ("nick", toJSON nick)]))
          mempty

  (GroupDMRemoveRecipient chan userid) ->
      Delete (channels // chan // chan /: "recipients" // userid) mempty

  (StartThreadFromMessage chan mid sto) ->
      Post (channels // chan /: "messages" // mid /: "threads")
           (pure $ R.ReqBodyJson $ toJSON sto)
           mempty

  (StartThreadNoMessage chan sto) ->
      Post (channels // chan /: "messages" /: "threads")
           (pure $ R.ReqBodyJson $ toJSON sto)
           mempty

  (JoinThread chan) ->
      Put (channels // chan /: "thread-members" /: "@me")
          R.NoReqBody mempty

  (AddThreadMember chan uid) ->
      Put (channels // chan /: "thread-members" // uid)
          R.NoReqBody mempty

  (LeaveThread chan) ->
      Delete (channels // chan /: "thread-members" /: "@me")
          mempty

  (RemoveThreadMember chan uid) ->
      Delete (channels // chan /: "thread-members" // uid)
          mempty

  (GetThreadMember chan uid) ->
      Get (channels // chan /: "thread-members" // uid)
          mempty

  (ListThreadMembers chan) ->
      Get (channels // chan /: "thread-members")
          mempty

  (ListPublicArchivedThreads chan (time, lim)) ->
      Get (channels // chan /: "threads" /: "archived" /: "public")
          (maybe mempty ("limit" R.=:) lim <> maybe mempty ("before" R.=:) time)

  (ListPrivateArchivedThreads chan (time, lim)) ->
      Get (channels // chan /: "threads" /: "archived" /: "private")
          (maybe mempty ("limit" R.=:) lim <> maybe mempty ("before" R.=:) time)

  (ListJoinedPrivateArchivedThreads chan (time, lim)) ->
      Get (channels // chan /: "users" /: "@me" /: "threads" /: "archived" /: "private")
          (maybe mempty ("limit" R.=:) lim <> maybe mempty ("before" R.=:) time)
