{-# LANGUAGE OverloadedStrings #-}

-- | Data structures pertaining to gateway dispatch 'Event's
module Discord.Types.Events where

import Prelude hiding (id)

import Data.Time.ISO8601 (parseISO8601)
import Data.Time (UTCTime)

import Control.Applicative ((<|>))
import Data.Aeson
import Data.Aeson.Types
import qualified Data.Text as T

import Discord.Types.Prelude (Snowflake)
import Discord.Types.Channel
import Discord.Types.Guild (Guild, Unavailable, GuildMember, Role, Emoji)


-- |Represents possible events sent by discord. Detailed information can be found at https://discordapp.com/developers/docs/topics/gateway.
data Event =
    Ready                   Int User [Channel] [Unavailable] String
  | Resumed                 [T.Text]
  | ChannelCreate           Channel
  | ChannelUpdate           Channel
  | ChannelDelete           Channel
  | ChannelPinsUpdate       Snowflake (Maybe UTCTime)
  | GuildCreate             Guild
  | GuildUpdate             Guild
  | GuildDelete             Unavailable
  | GuildBanAdd             Snowflake User
  | GuildBanRemove          Snowflake User
  | GuildEmojiUpdate        Snowflake [Emoji]
  | GuildIntegrationsUpdate Snowflake
  | GuildMemberAdd          Snowflake GuildMember
  | GuildMemberRemove       Snowflake User
  | GuildMemberUpdate       Snowflake [Snowflake] User String
  | GuildMemberChunk        Snowflake [GuildMember]
  | GuildRoleCreate         Snowflake Role
  | GuildRoleUpdate         Snowflake Role
  | GuildRoleDelete         Snowflake Snowflake
  | MessageCreate           Message
  | MessageUpdate           Message
  | MessageDelete           Snowflake Snowflake
  | MessageDeleteBulk       Snowflake [Snowflake]
  | MessageReactionAdd      ReactionInfo
  | MessageReactionRemove   ReactionInfo
  | MessageReactionRemoveAll Snowflake Snowflake
  | PresenceUpdate          PresenceInfo
  | TypingStart             TypingInfo
  | UserUpdate              User
  -- | VoiceStateUpdate
  -- | VoiceServerUpdate
  | UnknownEvent     String Object
  deriving Show

data ReactionInfo = ReactionInfo
  { reactionUserId    :: Snowflake
  , reactionChannelId :: Snowflake
  , reactionMessageId :: Snowflake
  , reactionEmoji     :: Emoji
  } deriving Show

instance FromJSON ReactionInfo where
  parseJSON = withObject "ReactionInfo" $ \o ->
    ReactionInfo <$> o .: "user_id"
                 <*> o .: "channel_id"
                 <*> o .: "message_id"
                 <*> o .: "emoji"

data PresenceInfo = PresenceInfo
  { presenceUserId  :: Snowflake
  , presenceRoles   :: [Role]
  -- , presenceGame :: Maybe Activity
  , presenceGuildId :: Snowflake
  , presencetatus   :: String
  } deriving Show

instance FromJSON PresenceInfo where
  parseJSON = withObject "PresenceInfo" $ \o ->
    PresenceInfo <$> (o .: "user" >>= (.: "id"))
                 <*> o .: "roles"
              -- <*> o .: "game"
                 <*> o .: "guild_id"
                 <*> o .: "status"

data TypingInfo = TypingInfo
  { typingUserId    :: Snowflake
  , typingChannelId :: Snowflake
  , typingTimestamp :: UTCTime
  } deriving Show

isoTimestamp :: Object -> T.Text -> Parser String
isoTimestamp o name = (o .: name :: Parser String) <|>
            (show <$> (o .: name :: Parser Integer))

instance FromJSON TypingInfo where
  parseJSON = withObject "TypingInfo" $ \o ->
    do cid <- o .: "channel_id"
       uid <- o .: "user_id"
       stamp <- isoTimestamp o "timestamp"
       case parseISO8601 stamp of
         Just utc -> pure (TypingInfo uid cid utc)
         Nothing -> fail "Invalid timestamp"



-- | Convert ToJSON value to FromJSON value
reparse :: (ToJSON a, FromJSON b) => a -> Parser b
reparse val = case parseEither parseJSON $ toJSON val of
                Left r -> fail r
                Right b -> pure b

eventParse :: T.Text -> Object -> Parser Event
eventParse t o = case t of
    "READY"                     -> Ready <$> o .: "v"
                                         <*> o .: "user"
                                         <*> o .: "private_channels"
                                         <*> o .: "guilds"
                                         <*> o .: "session_id"
    "RESUMED"                   -> Resumed <$> o .: "_trace"
    "CHANNEL_CREATE"            -> ChannelCreate             <$> reparse o
    "CHANNEL_UPDATE"            -> ChannelUpdate             <$> reparse o
    "CHANNEL_DELETE"            -> ChannelDelete             <$> reparse o
    "CHANNEL_PINS_UPDATE"       -> do id <- o .: "channel_id"
                                      stamp <- isoTimestamp o "last_pin_timestamp"
                                      let utc = parseISO8601 stamp
                                      pure (ChannelPinsUpdate id utc)
    "GUILD_CREATE"              -> GuildCreate               <$> reparse o
    "GUILD_UPDATE"              -> GuildUpdate               <$> reparse o
    "GUILD_DELETE"              -> GuildDelete               <$> reparse o
    "GUILD_BAN_ADD"             -> GuildBanAdd    <$> o .: "guild_id" <*> reparse o
    "GUILD_BAN_REMOVE"          -> GuildBanRemove <$> o .: "guild_id" <*> reparse o
    "GUILD_EMOJI_UPDATE"        -> GuildEmojiUpdate <$> o .: "guild_id" <*> o .: "emojis"
    "GUILD_INTEGRATIONS_UPDATE" -> GuildIntegrationsUpdate   <$> o .: "guild_id"
    "GUILD_MEMBER_ADD"          -> GuildMemberAdd <$> o .: "guild_id" <*> reparse o
    "GUILD_MEMBER_REMOVE"       -> GuildMemberRemove <$> o .: "guild_id" <*> o .: "user"
    "GUILD_MEMBER_UPDATE"       -> GuildMemberUpdate <$> o .: "guild_id"
                                                     <*> o .: "roles"
                                                     <*> o .: "user"
                                                     <*> o .: "nick"
    "GUILD_MEMBER_CHUNK"        -> GuildMemberChunk <$> o .: "guild_id" <*> o .: "members"
    "GUILD_ROLE_CREATE"         -> GuildRoleCreate  <$> o .: "guild_id" <*> o .: "role"
    "GUILD_ROLE_UPDATE"         -> GuildRoleUpdate  <$> o .: "guild_id" <*> o .: "role"
    "GUILD_ROLE_DELETE"         -> GuildRoleDelete  <$> o .: "guild_id" <*> o .: "role"
    "MESSAGE_CREATE"            -> MessageCreate             <$> reparse o
    "MESSAGE_UPDATE"            -> MessageUpdate             <$> reparse o
    "MESSAGE_DELETE"            -> MessageDelete     <$> o .: "channel_id" <*> o .: "id"
    "MESSAGE_DELETE_BULK"       -> MessageDeleteBulk <$> o .: "channel_id" <*> o .: "ids"
    "MESSAGE_REACTION_ADD"      -> MessageReactionAdd <$> reparse o
    "MESSAGE_REACTION_REMOVE"   -> MessageReactionRemove <$> reparse o
    "MESSAGE_REACTION_REMOVE_ALL" -> MessageReactionRemoveAll <$> o .: "channel_id"
                                                              <*> o .: "message_id"
    "PRESENCE_UPDATE"           -> PresenceUpdate            <$> reparse o
    "TYPING_START"              -> TypingStart               <$> reparse o
    "USER_UPDATE"               -> UserUpdate                <$> reparse o
 -- "VOICE_STATE_UPDATE"        -> VoiceStateUpdate          <$> reparse o
 -- "VOICE_SERVER_UPDATE"       -> VoiceServerUpdate         <$> reparse o
    _other_event                -> UnknownEvent (T.unpack t) <$> reparse o
