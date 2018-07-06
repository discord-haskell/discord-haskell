{-# LANGUAGE OverloadedStrings #-}

-- | Data structures pertaining to gateway dispatch 'Event's
module Discord.Types.Events where

import Prelude hiding (id)
import Control.Monad (mzero)

import Data.Time.ISO8601 (parseISO8601)
import Data.Time (UTCTime)

import Data.Aeson
import Data.Aeson.Types
import qualified Data.Text as T

import Discord.Types.Prelude (Snowflake)
import Discord.Types.Channel
import Discord.Types.Guild (Guild, Unavailable)


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
  | GuildEmojiUpdate        Object
  | GuildIntegrationsUpdate Object
  | GuildMemberAdd          Member
  | GuildMemberRemove       Member
  | GuildMemberUpdate       Member
  | GuildMemberChunk        Object
  | GuildRoleCreate         Object
  | GuildRoleUpdate         Object
  | GuildRoleDelete         Object
  | MessageCreate           Message
  | MessageUpdate           Message
  | MessageDelete           Object
  | MessageDeleteBulk       Object
  | PresenceUpdate          Object
  | TypingStart             Object
  | UserSettingsUpdate      Object
  | UserUpdate              Object
  | VoiceStateUpdate        Object
  | VoiceServerUpdate       Object
  | UnknownEvent     String Object
  deriving Show

-- | Convert ToJSON value to FromJSON value
reparse :: (ToJSON a, FromJSON b) => a -> Parser b
reparse val = case parseEither parseJSON $ toJSON val of
                Left _ -> mzero
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
                                      stamp <- o .:? "last_pin_timestamp"
                                      let utc = stamp >>= parseISO8601
                                      pure (ChannelPinsUpdate id utc)
    "GUILD_CREATE"              -> GuildCreate               <$> reparse o
    "GUILD_UPDATE"              -> GuildUpdate               <$> reparse o
    "GUILD_DELETE"              -> GuildDelete               <$> reparse o
    "GUILD_BAN_ADD"             -> GuildBanAdd    <$> o .: "guild_id" <*> reparse o
    "GUILD_BAN_REMOVE"          -> GuildBanRemove <$> o .: "guild_id" <*> reparse o
    "GUILD_EMOJI_UPDATE"        -> GuildEmojiUpdate          <$> reparse o
    "GUILD_INTEGRATIONS_UPDATE" -> GuildIntegrationsUpdate   <$> reparse o
    "GUILD_MEMBER_ADD"          -> GuildMemberAdd            <$> reparse o
    "GUILD_MEMBER_UPDATE"       -> GuildMemberUpdate         <$> reparse o
    "GUILD_MEMBER_REMOVE"       -> GuildMemberRemove         <$> reparse o
    "GUILD_MEMBER_CHUNK"        -> GuildMemberChunk          <$> reparse o
    "GUILD_ROLE_CREATE"         -> GuildRoleCreate           <$> reparse o
    "GUILD_ROLE_UPDATE"         -> GuildRoleUpdate           <$> reparse o
    "GUILD_ROLE_DELETE"         -> GuildRoleDelete           <$> reparse o
    "MESSAGE_CREATE"            -> MessageCreate             <$> reparse o
    "MESSAGE_UPDATE"            -> MessageUpdate             <$> reparse o
    "MESSAGE_DELETE"            -> MessageDelete             <$> reparse o
    "MESSAGE_DELETE_BULK"       -> MessageDeleteBulk         <$> reparse o
    "PRESENCE_UPDATE"           -> PresenceUpdate            <$> reparse o
    "TYPING_START"              -> TypingStart               <$> reparse o
    "USER_SETTINGS_UPDATE"      -> UserSettingsUpdate        <$> reparse o
    "VOICE_STATE_UPDATE"        -> VoiceStateUpdate          <$> reparse o
    "VOICE_SERVER_UPDATE"       -> VoiceServerUpdate         <$> reparse o
    _other_event                -> UnknownEvent (T.unpack t) <$> reparse o
