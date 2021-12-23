{-# LANGUAGE OverloadedStrings #-}

-- | Data structures pertaining to gateway dispatch 'Event's
module Discord.Internal.Types.Events where

import Prelude hiding (id)

import Data.Time.ISO8601 (parseISO8601)
import Data.Time (UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)

import Data.Aeson
import Data.Aeson.Types
import qualified Data.Text as T

import Discord.Internal.Types.Prelude
import Discord.Internal.Types.Channel
import Discord.Internal.Types.Guild     ( Role, GuildInfo, GuildUnavailable, Guild )
import Discord.Internal.Types.User (User, GuildMember)
import Discord.Internal.Types.Interactions (InternalInteraction, Interaction)
import Discord.Internal.Types.Components (Emoji)


-- | Represents possible events sent by discord. Detailed information can be found at https://discord.com/developers/docs/topics/gateway.
data Event =
    Ready                   Int User [Channel] [GuildUnavailable] T.Text (Maybe Shard) PartialApplication
  | Resumed                 [T.Text]
  | ChannelCreate           Channel
  | ChannelUpdate           Channel
  | ChannelDelete           Channel
  | ChannelPinsUpdate       ChannelId (Maybe UTCTime)
  | GuildCreate             Guild GuildInfo
  | GuildUpdate             Guild
  | GuildDelete             GuildUnavailable
  | GuildBanAdd             GuildId User
  | GuildBanRemove          GuildId User
  | GuildEmojiUpdate        GuildId [Emoji]
  | GuildIntegrationsUpdate GuildId
  | GuildMemberAdd          GuildId GuildMember
  | GuildMemberRemove       GuildId User
  | GuildMemberUpdate       GuildId [RoleId] User (Maybe T.Text)
  | GuildMemberChunk        GuildId [GuildMember]
  | GuildRoleCreate         GuildId Role
  | GuildRoleUpdate         GuildId Role
  | GuildRoleDelete         GuildId RoleId
  | MessageCreate           Message
  | MessageUpdate           ChannelId MessageId
  | MessageDelete           ChannelId MessageId
  | MessageDeleteBulk       ChannelId [MessageId]
  | MessageReactionAdd      ReactionInfo
  | MessageReactionRemove   ReactionInfo
  | MessageReactionRemoveAll ChannelId MessageId
  | MessageReactionRemoveEmoji ReactionRemoveInfo
  | PresenceUpdate          PresenceInfo
  | TypingStart             TypingInfo
  | UserUpdate              User
  | InteractionCreate       Interaction
  -- | VoiceStateUpdate
  -- | VoiceServerUpdate
  | UnknownEvent     T.Text Object
  deriving (Show, Read, Eq)

data EventInternalParse =
    InternalReady                   Int User [Channel] [GuildUnavailable] T.Text (Maybe Shard) PartialApplication
  | InternalResumed                 [T.Text]
  | InternalChannelCreate           Channel
  | InternalChannelUpdate           Channel
  | InternalChannelDelete           Channel
  | InternalChannelPinsUpdate       ChannelId (Maybe UTCTime)
  | InternalGuildCreate             Guild GuildInfo
  | InternalGuildUpdate             Guild
  | InternalGuildDelete             GuildUnavailable
  | InternalGuildBanAdd             GuildId User
  | InternalGuildBanRemove          GuildId User
  | InternalGuildEmojiUpdate        GuildId [Emoji]
  | InternalGuildIntegrationsUpdate GuildId
  | InternalGuildMemberAdd          GuildId GuildMember
  | InternalGuildMemberRemove       GuildId User
  | InternalGuildMemberUpdate       GuildId [RoleId] User (Maybe T.Text)
  | InternalGuildMemberChunk        GuildId [GuildMember]
  | InternalGuildRoleCreate         GuildId Role
  | InternalGuildRoleUpdate         GuildId Role
  | InternalGuildRoleDelete         GuildId RoleId
  | InternalMessageCreate           Message
  | InternalMessageUpdate           ChannelId MessageId
  | InternalMessageDelete           ChannelId MessageId
  | InternalMessageDeleteBulk       ChannelId [MessageId]
  | InternalMessageReactionAdd      ReactionInfo
  | InternalMessageReactionRemove   ReactionInfo
  | InternalMessageReactionRemoveAll ChannelId MessageId
  | InternalMessageReactionRemoveEmoji ReactionRemoveInfo
  | InternalPresenceUpdate          PresenceInfo
  | InternalTypingStart             TypingInfo
  | InternalUserUpdate              User
  | InternalInteractionCreate       InternalInteraction
  -- | InternalVoiceStateUpdate
  -- | InternalVoiceServerUpdate
  | InternalUnknownEvent     T.Text Object
  deriving (Show, Read, Eq)

data PartialApplication = PartialApplication
  { partialApplicationID :: ApplicationId
  , partialApplicationFlags :: Int
  } deriving (Show, Eq, Read)

instance FromJSON PartialApplication where
  parseJSON = withObject "PartialApplication" (\v -> PartialApplication <$> v .: "id" <*> v .: "flags")

data ReactionInfo = ReactionInfo
  { reactionUserId    :: UserId
  , reactionGuildId   :: Maybe GuildId
  , reactionChannelId :: ChannelId
  , reactionMessageId :: MessageId
  , reactionEmoji     :: Emoji
  } deriving (Show, Read, Eq, Ord)

instance FromJSON ReactionInfo where
  parseJSON = withObject "ReactionInfo" $ \o ->
    ReactionInfo <$> o .:  "user_id"
                 <*> o .:? "guild_id"
                 <*> o .:  "channel_id"
                 <*> o .:  "message_id"
                 <*> o .:  "emoji"

data ReactionRemoveInfo  = ReactionRemoveInfo
  { reactionRemoveChannelId :: ChannelId
  , reactionRemoveGuildId   :: GuildId
  , reactionRemoveMessageId :: MessageId
  , reactionRemoveEmoji     :: Emoji
  } deriving (Show, Read, Eq, Ord)

instance FromJSON ReactionRemoveInfo where
  parseJSON = withObject "ReactionRemoveInfo" $ \o ->
    ReactionRemoveInfo <$> o .:  "guild_id"
                       <*> o .:  "channel_id"
                       <*> o .:  "message_id"
                       <*> o .:  "emoji"

data PresenceInfo = PresenceInfo
  { presenceUserId  :: UserId
  , presenceRoles   :: [RoleId]
  -- , presenceGame :: Maybe Activity
  , presenceGuildId :: GuildId
  , presenceStatus  :: T.Text
  } deriving (Show, Read, Eq, Ord)

instance FromJSON PresenceInfo where
  parseJSON = withObject "PresenceInfo" $ \o ->
    PresenceInfo <$> (o .: "user" >>= (.: "id"))
                 <*> o .: "roles"
              -- <*> o .: "game"
                 <*> o .: "guild_id"
                 <*> o .: "status"

data TypingInfo = TypingInfo
  { typingUserId    :: UserId
  , typingChannelId :: ChannelId
  , typingTimestamp :: UTCTime
  } deriving (Show, Read, Eq, Ord)

instance FromJSON TypingInfo where
  parseJSON = withObject "TypingInfo" $ \o ->
    do cid <- o .: "channel_id"
       uid <- o .: "user_id"
       posix <- o .: "timestamp"
       let utc = posixSecondsToUTCTime posix
       pure (TypingInfo uid cid utc)



-- | Convert ToJSON value to FromJSON value
reparse :: (ToJSON a, FromJSON b) => a -> Parser b
reparse val = case parseEither parseJSON $ toJSON val of
                Left r -> fail r
                Right b -> pure b

eventParse :: T.Text -> Object -> Parser EventInternalParse
eventParse t o = case t of
    "READY"                     -> InternalReady <$> o .: "v"
                                         <*> o .: "user"
                                         <*> o .: "private_channels"
                                         <*> o .: "guilds"
                                         <*> o .: "session_id"
                                         <*> o .: "shard"
                                         <*> o .: "application"
    "RESUMED"                   -> InternalResumed <$> o .: "_trace"
    "CHANNEL_CREATE"            -> InternalChannelCreate             <$> reparse o
    "CHANNEL_UPDATE"            -> InternalChannelUpdate             <$> reparse o
    "CHANNEL_DELETE"            -> InternalChannelDelete             <$> reparse o
    "CHANNEL_PINS_UPDATE"       -> do id <- o .: "channel_id"
                                      stamp <- o .:? "last_pin_timestamp"
                                      let utc = stamp >>= parseISO8601
                                      pure (InternalChannelPinsUpdate id utc)
    "GUILD_CREATE"              -> InternalGuildCreate               <$> reparse o <*> reparse o
    "GUILD_UPDATE"              -> InternalGuildUpdate               <$> reparse o
    "GUILD_DELETE"              -> InternalGuildDelete               <$> reparse o
    "GUILD_BAN_ADD"             -> InternalGuildBanAdd    <$> o .: "guild_id" <*> o .: "user"
    "GUILD_BAN_REMOVE"          -> InternalGuildBanRemove <$> o .: "guild_id" <*> o .: "user"
    "GUILD_EMOJI_UPDATE"        -> InternalGuildEmojiUpdate <$> o .: "guild_id" <*> o .: "emojis"
    "GUILD_INTEGRATIONS_UPDATE" -> InternalGuildIntegrationsUpdate   <$> o .: "guild_id"
    "GUILD_MEMBER_ADD"          -> InternalGuildMemberAdd <$> o .: "guild_id" <*> reparse o
    "GUILD_MEMBER_REMOVE"       -> InternalGuildMemberRemove <$> o .: "guild_id" <*> o .: "user"
    "GUILD_MEMBER_UPDATE"       -> InternalGuildMemberUpdate <$> o .: "guild_id"
                                                             <*> o .: "roles"
                                                             <*> o .: "user"
                                                             <*> o .:? "nick"
    "GUILD_MEMBERS_CHUNK"       -> InternalGuildMemberChunk <$> o .: "guild_id" <*> o .: "members"
    "GUILD_ROLE_CREATE"         -> InternalGuildRoleCreate  <$> o .: "guild_id" <*> o .: "role"
    "GUILD_ROLE_UPDATE"         -> InternalGuildRoleUpdate  <$> o .: "guild_id" <*> o .: "role"
    "GUILD_ROLE_DELETE"         -> InternalGuildRoleDelete  <$> o .: "guild_id" <*> o .: "role_id"
    "MESSAGE_CREATE"            -> InternalMessageCreate     <$> reparse o
    "MESSAGE_UPDATE"            -> InternalMessageUpdate     <$> o .: "channel_id" <*> o .: "id"
    "MESSAGE_DELETE"            -> InternalMessageDelete     <$> o .: "channel_id" <*> o .: "id"
    "MESSAGE_DELETE_BULK"       -> InternalMessageDeleteBulk <$> o .: "channel_id" <*> o .: "ids"
    "MESSAGE_REACTION_ADD"      -> InternalMessageReactionAdd <$> reparse o
    "MESSAGE_REACTION_REMOVE"   -> InternalMessageReactionRemove <$> reparse o
    "MESSAGE_REACTION_REMOVE_ALL" -> InternalMessageReactionRemoveAll <$> o .: "channel_id"
                                                                      <*> o .: "message_id"
    "MESSAGE_REACTION_REMOVE_EMOJI"-> InternalMessageReactionRemoveEmoji <$> reparse o
    "PRESENCE_UPDATE"           -> InternalPresenceUpdate            <$> reparse o
    "TYPING_START"              -> InternalTypingStart               <$> reparse o
    "USER_UPDATE"               -> InternalUserUpdate                <$> reparse o
 -- "VOICE_STATE_UPDATE"        -> InternalVoiceStateUpdate          <$> reparse o
 -- "VOICE_SERVER_UPDATE"       -> InternalVoiceServerUpdate         <$> reparse o
    "INTERACTION_CREATE"        -> InternalInteractionCreate         <$> reparse o
    _other_event                -> InternalUnknownEvent t            <$> reparse o
