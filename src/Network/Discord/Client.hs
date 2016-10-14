module Network.Discord.Client where
  import Control.Concurrent.STM
  import Network.Discord.Gateway
  
  import Data.Aeson

  class Client a where
    getAuth :: (Client a) => a -> Auth
    
    onReady :: (Client a) => a -> TBQueue Payload -> Object -> IO a
    onReady a _ _ = return a

    onResume :: (Client a) => a -> TBQueue Payload -> Object -> IO a
    onResume a _ _ = return a

    onChannelCreate :: (Client a) => a -> TBQueue Payload -> Object -> IO a
    onChannelCreate a _ _ = return a

    onChannelUpdate :: (Client a) => a -> TBQueue Payload -> Object -> IO a
    onChannelUpdate a _ _ = return a

    onChannelDelete :: (Client a) => a -> TBQueue Payload -> Object -> IO a
    onChannelDelete a _ _ = return a

    onGuildCreate :: (Client a) => a -> TBQueue Payload -> Object -> IO a
    onGuildCreate a _ _ = return a

    onGuildUpdate :: (Client a) => a -> TBQueue Payload -> Object -> IO a
    onGuildUpdate a _ _ = return a

    onGuildDelete :: (Client a) => a -> TBQueue Payload -> Object -> IO a
    onGuildDelete a _ _ = return a

    onGuildBanAdd :: (Client a) => a -> TBQueue Payload -> Object -> IO a
    onGuildBanAdd a _ _ = return a

    onGuildBanRemove :: (Client a) => a -> TBQueue Payload -> Object -> IO a
    onGuildBanRemove a _ _ = return a

    onGuildEmojiUpdate :: (Client a) => a -> TBQueue Payload -> Object -> IO a
    onGuildEmojiUpdate a _ _ = return a

    onGuildIntegrationsUpdate :: (Client a) => a -> TBQueue Payload -> Object -> IO a
    onGuildIntegrationsUpdate a _ _ = return a

    onGuildMemberAdd :: (Client a) => a -> TBQueue Payload -> Object -> IO a
    onGuildMemberAdd a _ _ = return a

    onGuildMemberRemove :: (Client a) => a -> TBQueue Payload -> Object -> IO a
    onGuildMemberRemove a _ _ = return a

    onGuildMemberUpdate :: (Client a) => a -> TBQueue Payload -> Object -> IO a
    onGuildMemberUpdate a _ _ = return a

    onGuildMembersChunk :: (Client a) => a -> TBQueue Payload -> Object -> IO a
    onGuildMembersChunk a _ _ = return a

    onGuildRoleCreate :: (Client a) => a -> TBQueue Payload -> Object -> IO a
    onGuildRoleCreate a _ _ = return a

    onGuildRoleUpdate :: (Client a) => a -> TBQueue Payload -> Object -> IO a
    onGuildRoleUpdate a _ _ = return a

    onGuildRoleDelete :: (Client a) => a -> TBQueue Payload -> Object -> IO a
    onGuildRoleDelete a _ _ = return a

    onMessageCreate :: (Client a) => a -> TBQueue Payload -> Object -> IO a
    onMessageCreate a _ _ = return a

    onMessageUpdate :: (Client a) => a -> TBQueue Payload -> Object -> IO a
    onMessageUpdate a _ _ = return a

    onMessageDelete :: (Client a) => a -> TBQueue Payload -> Object -> IO a
    onMessageDelete a _ _ = return a

    onMessageDeleteBulk :: (Client a) => a -> TBQueue Payload -> Object -> IO a
    onMessageDeleteBulk a _ _ = return a

    onPresenceUpdate :: (Client a) => a -> TBQueue Payload -> Object -> IO a
    onPresenceUpdate a _ _ = return a

    onTypingStart :: (Client a) => a -> TBQueue Payload -> Object -> IO a
    onTypingStart a _ _ = return a

    onUserSettingsUpdate :: (Client a) => a -> TBQueue Payload -> Object -> IO a
    onUserSettingsUpdate a _ _ = return a

    onUserUpdate :: (Client a) => a -> TBQueue Payload -> Object -> IO a
    onUserUpdate a _ _ = return a

    onVoiceStateUpdate :: (Client a) => a -> TBQueue Payload -> Object -> IO a
    onVoiceStateUpdate a _ _ = return a

    onVoiceServerUpdate :: (Client a) => a -> TBQueue Payload -> Object -> IO a
    onVoiceServerUpdate a _ _ = return a
