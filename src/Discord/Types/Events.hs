{-# LANGUAGE OverloadedStrings #-}

-- | Data structures pertaining to gateway dispatch 'Event's
module Discord.Types.Events where

import Control.Monad (mzero)

import Data.Aeson
import Data.Aeson.Types
import qualified Data.Text as T

import Discord.Types.Channel
import Discord.Types.Guild (Member, Guild)

-- |Represents data sent on READY event.
data Init = Init Int User [Channel] [Guild] String deriving Show

-- |Allows Init type to be generated using a JSON response by Discord.
instance FromJSON Init where
  parseJSON (Object o) = Init <$> o .: "v"
                              <*> o .: "user"
                              <*> o .: "private_channels"
                              <*> o .: "guilds"
                              <*> o .: "session_id"
  parseJSON _          = mzero

-- |Represents possible events sent by discord. Detailed information can be found at https://discordapp.com/developers/docs/topics/gateway.
data Event =
    Ready                   Init
  | Resumed                 Object
  | ChannelCreate           Channel
  | ChannelUpdate           Channel
  | ChannelDelete           Channel
  | GuildCreate             Guild
  | GuildUpdate             Guild
  | GuildDelete             Guild
  | GuildBanAdd             Member
  | GuildBanRemove          Member
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
  | Nil
  deriving Show

-- | Convert ToJSON value to FromJSON value
reparse :: (ToJSON a, FromJSON b) => a -> Parser b
reparse val = case parseEither parseJSON $ toJSON val of
                Left err -> mzero
                Right b -> pure b

eventParse :: T.Text -> Object -> Parser Event
eventParse t o = case t of
    "RESUMED"                   -> Resumed                 <$> reparse o
    "RESUMED"                   -> Resumed                 <$> reparse o
    "CHANNEL_CREATE"            -> ChannelCreate           <$> reparse o
    "CHANNEL_UPDATE"            -> ChannelUpdate           <$> reparse o
    "CHANNEL_DELETE"            -> ChannelDelete           <$> reparse o
    "GUILD_CREATE"              -> GuildCreate             <$> reparse o
    "GUILD_UPDATE"              -> GuildUpdate             <$> reparse o
    "GUILD_DELETE"              -> GuildDelete             <$> reparse o
    "GUILD_BAN_ADD"             -> GuildBanAdd             <$> reparse o
    "GUILD_BAN_REMOVE"          -> GuildBanRemove          <$> reparse o
    "GUILD_EMOJI_UPDATE"        -> GuildEmojiUpdate        <$> reparse o
    "GUILD_INTEGRATIONS_UPDATE" -> GuildIntegrationsUpdate <$> reparse o
    "GUILD_MEMBER_ADD"          -> GuildMemberAdd          <$> reparse o
    "GUILD_MEMBER_UPDATE"       -> GuildMemberUpdate       <$> reparse o
    "GUILD_MEMBER_REMOVE"       -> GuildMemberRemove       <$> reparse o
    "GUILD_MEMBER_CHUNK"        -> GuildMemberChunk        <$> reparse o
    "GUILD_ROLE_CREATE"         -> GuildRoleCreate         <$> reparse o
    "GUILD_ROLE_UPDATE"         -> GuildRoleUpdate         <$> reparse o
    "GUILD_ROLE_DELETE"         -> GuildRoleDelete         <$> reparse o
    "MESSAGE_CREATE"            -> MessageCreate           <$> reparse o
    "MESSAGE_UPDATE"            -> MessageUpdate           <$> reparse o
    "MESSAGE_DELETE"            -> MessageDelete           <$> reparse o
    "MESSAGE_DELETE_BULK"       -> MessageDeleteBulk       <$> reparse o
    "PRESENCE_UPDATE"           -> PresenceUpdate          <$> reparse o
    "TYPING_START"              -> TypingStart             <$> reparse o
    "USER_SETTINGS_UPDATE"      -> UserSettingsUpdate      <$> reparse o
    "VOICE_STATE_UPDATE"        -> VoiceStateUpdate        <$> reparse o
    "VOICE_SERVER_UPDATE"       -> VoiceServerUpdate       <$> reparse o
    _                           -> UnknownEvent t          <$> reparse o

