{-# LANGUAGE OverloadedStrings, GADTs #-}
-- | Data structures pertaining to gateway dispatch 'Event's
module Network.Discord.Types.Events where
  import Control.Monad (mzero)

  import Data.Aeson

  import Network.Discord.Types.Channel
  import Network.Discord.Types.Gateway
  import Network.Discord.Types.Guild (Member, Guild)
  import Network.Discord.Types.Prelude

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
    deriving Show

  -- |Parses JSON stuff by Discord to an event type.
  parseDispatch :: Payload -> Either String Event
  parseDispatch (Dispatch ob _ ev) = case ev of
    "READY"                     -> Ready                   <$> reparse o
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
    _                           -> UnknownEvent ev         <$> reparse o
    where o = Object ob
  parseDispatch _ = error "Tried to parse non-Dispatch payload"

