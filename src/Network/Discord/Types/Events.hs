{-# LANGUAGE OverloadedStrings, GADTs #-}
module Network.Discord.Types.Events where
  import Control.Monad (mzero)

  import Data.Aeson

  import Network.Discord.Types.Json
  import Network.Discord.Types.Gateway

  data Init = Init Int User [Channel] [Guild] String deriving Show
  instance FromJSON Init where
    parseJSON (Object o) = Init <$> o .: "v"
                                <*> o .: "user"
                                <*> o .: "private_channels"
                                <*> o .: "guilds"
                                <*> o .: "session_id"
    parseJSON _          = mzero

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
    deriving Show

  parseDispatch :: Payload -> Event
  parseDispatch (Dispatch ob _ ev) = case ev of
    "READY"                     -> Ready                   $ reparse o
    "RESUMED"                   -> Resumed                 $ reparse o
    "CHANNEL_CREATE"            -> ChannelCreate           $ reparse o
    "CHANNEL_UPDATE"            -> ChannelUpdate           $ reparse o
    "CHANNEL_DELETE"            -> ChannelDelete           $ reparse o
    "GUILD_CREATE"              -> GuildCreate             $ reparse o
    "GUILD_UPDATE"              -> GuildUpdate             $ reparse o
    "GUILD_DELETE"              -> GuildDelete             $ reparse o
    "GUILD_BAN_ADD"             -> GuildBanAdd             $ reparse o
    "GUILD_BAN_REMOVE"          -> GuildBanRemove          $ reparse o
    "GUILD_EMOJI_UPDATE"        -> GuildEmojiUpdate        $ reparse o
    "GUILD_INTEGRATIONS_UPDATE" -> GuildIntegrationsUpdate $ reparse o
    "GUILD_MEMBER_ADD"          -> GuildMemberAdd          $ reparse o
    "GUILD_MEMBER_REMOVE"       -> GuildMemberRemove       $ reparse o
    "GUILD_MEMBER_CHUNK"        -> GuildMemberChunk        $ reparse o
    "GUILD_ROLE_CREATE"         -> GuildRoleCreate         $ reparse o
    "GUILD_ROLE_UPDATE"         -> GuildRoleUpdate         $ reparse o
    "GUILD_ROLE_DELETE"         -> GuildRoleDelete         $ reparse o
    "MESSAGE_CREATE"            -> MessageCreate           $ reparse o
    "MESSAGE_UPDATE"            -> MessageUpdate           $ reparse o
    "MESSAGE_DELETE"            -> MessageDelete           $ reparse o
    "MESSAGE_DELETE_BULK"       -> MessageDeleteBulk       $ reparse o
    "PRESENCE_UPDATE"           -> PresenceUpdate          $ reparse o
    "TYPING_START"              -> TypingStart             $ reparse o
    "USER_SETTINGS_UPDATE"      -> UserSettingsUpdate      $ reparse o
    "VOICE_STATE_UPDATE"        -> VoiceStateUpdate        $ reparse o
    "VOICE_SERVER_UPDATE"       -> VoiceServerUpdate       $ reparse o
    _                           -> UnknownEvent ev         $ reparse o
    where o = Object ob
  parseDispatch _ = error "Tried to parse non-Dispatch payload"
