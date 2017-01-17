{-# LANGUAGE TypeFamilies, ExistentialQuantification, RankNTypes, MultiParamTypeClasses #-}
module Language.Discord where
  import Control.Monad.Writer
  import Control.Monad.IO.Class

  import Pipes.Core
  import Network.Discord as D
  
  data BotClient = BotClient Auth
  instance D.Client BotClient where
    getAuth (BotClient auth) = auth

  runBot :: Auth -> DiscordBot () -> IO ()
  runBot auth bot = do
    gateway <- getGateway
    runWebsocket gateway (BotClient auth) $ do
      DiscordState {getWebSocket=ws} <- get
      (eventCore ~> (handle $ execWriter bot)) ws
  
  type DiscordBot a = Writer Handle a

  data Handle = Null 
              | Misc (Event -> Effect DiscordM ())
              | ReadyEvent (Event -> Effect DiscordM ())
              | ResumedEvent (Event -> Effect DiscordM ())
              | ChannelCreateEvent (Event -> Effect DiscordM ())
              | ChannelUpdateEvent (Event -> Effect DiscordM ())
              | ChannelDeleteEvent (Event -> Effect DiscordM ())
              | GuildCreateEvent (Event -> Effect DiscordM ())
              | GuildUpdateEvent (Event -> Effect DiscordM ())
              | GuildDeleteEvent (Event -> Effect DiscordM ())
              | GuildBanAddEvent (Event -> Effect DiscordM ())
              | GuildBanRemoveEvent (Event -> Effect DiscordM ())
              | GuildEmojiUpdateEvent (Event -> Effect DiscordM ())
              | GuildIntegrationsUpdateEvent (Event -> Effect DiscordM ())
              | GuildMemberAddEvent (Event -> Effect DiscordM ())
              | GuildMemberRemoveEvent (Event -> Effect DiscordM ())
              | GuildMemberUpdateEvent (Event -> Effect DiscordM ())
              | GuildMemberChunkEvent (Event -> Effect DiscordM ())
              | GuildRoleCreateEvent (Event -> Effect DiscordM ())
              | GuildRoleUpdateEvent (Event -> Effect DiscordM ())
              | GuildRoleDeleteEvent (Event -> Effect DiscordM ())
              | MessageCreateEvent (Event -> Effect DiscordM ())
              | MessageUpdateEvent (Event -> Effect DiscordM ())
              | MessageDeleteEvent (Event -> Effect DiscordM ())
              | MessageDeleteBulkEvent (Event -> Effect DiscordM ())
              | PresenceUpdateEvent (Event -> Effect DiscordM ())
              | TypingStartEvent (Event -> Effect DiscordM ())
              | UserSettingsUpdateEvent (Event -> Effect DiscordM ())
              | UserUpdateEvent (Event -> Effect DiscordM ())
              | VoiceStateUpdateEvent (Event -> Effect DiscordM ())
              | VoiceServerUpdateEvent (Event -> Effect DiscordM ())
              | Event String (Event -> Effect DiscordM ())

  with :: (a -> Handle) -> a -> DiscordBot ()
  with f a = tell $ f a
  
  instance Monoid Handle where
    mempty = Null
    a `mappend` b = Misc ( \ev -> handle a ev <> handle b ev)

  handle :: Handle -> Event -> Effect DiscordM ()
  handle (Misc f) ev = f ev
  handle (ReadyEvent p) ev@(D.Ready _) = p ev
  handle (ResumedEvent p) ev@(D.Resumed _) = p ev
  handle (ChannelCreateEvent p) ev@(D.ChannelCreate _) = p ev
  handle (ChannelUpdateEvent p) ev@(D.ChannelUpdate _) = p ev
  handle (ChannelDeleteEvent p) ev@(D.ChannelDelete _) = p ev
  handle (GuildCreateEvent p) ev@(D.GuildCreate _) = p ev
  handle (GuildUpdateEvent p) ev@(D.GuildUpdate _) = p ev
  handle (GuildDeleteEvent p) ev@(D.GuildDelete _) = p ev
  handle (GuildBanAddEvent p) ev@(D.GuildBanAdd _) = p ev
  handle (GuildBanRemoveEvent p) ev@(D.GuildBanRemove _) = p ev
  handle (GuildEmojiUpdateEvent p) ev@(D.GuildEmojiUpdate _) = p ev
  handle (GuildIntegrationsUpdateEvent p) ev@(D.GuildIntegrationsUpdate _) = p ev
  handle (GuildMemberAddEvent p) ev@(D.GuildMemberAdd _) = p ev
  handle (GuildMemberRemoveEvent p) ev@(D.GuildMemberRemove _) = p ev
  handle (GuildMemberUpdateEvent p) ev@(D.GuildMemberUpdate _) = p ev
  handle (GuildMemberChunkEvent p) ev@(D.GuildMemberChunk _) = p ev
  handle (GuildRoleCreateEvent p) ev@(D.GuildRoleCreate _) = p ev
  handle (GuildRoleUpdateEvent p) ev@(D.GuildRoleUpdate _) = p ev
  handle (GuildRoleDeleteEvent p) ev@(D.GuildRoleDelete _) = p ev
  handle (MessageCreateEvent p) ev@(D.MessageCreate _) = p ev
  handle (MessageUpdateEvent p) ev@(D.MessageUpdate _) = p ev
  handle (MessageDeleteEvent p) ev@(D.MessageDelete _) = p ev
  handle (MessageDeleteBulkEvent p) ev@(D.MessageDeleteBulk _) = p ev
  handle (PresenceUpdateEvent p) ev@(D.PresenceUpdate _) = p ev
  handle (TypingStartEvent p) ev@(D.TypingStart _) = p ev
  handle (UserSettingsUpdateEvent p) ev@(D.UserSettingsUpdate _) = p ev
  handle (UserUpdateEvent p) ev@(D.UserUpdate _) = p ev
  handle (VoiceStateUpdateEvent p) ev@(D.VoiceStateUpdate _) = p ev
  handle (VoiceServerUpdateEvent p) ev@(D.VoiceServerUpdate _) = p ev
  handle (Event s p) ev@(D.UnknownEvent v _)
    | s == v = p ev
  handle _ ev = liftIO $ putStrLn $ "Miss event " ++ show ev
