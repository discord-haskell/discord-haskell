{-# LANGUAGE TypeFamilies, ExistentialQuantification, RankNTypes, MultiParamTypeClasses #-}
module Language.Discord where
  import Control.Monad.Writer

  import Pipes.Core
  import Data.Aeson (Object)

  import Network.Discord as D
  import Network.Discord.Types.Guild (Member, Guild)
  
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
              | Misc                         (Event -> Effect DiscordM ())
              | ReadyEvent                   (Init -> Effect DiscordM ())
              | ResumedEvent                 (Object -> Effect DiscordM ())
              | ChannelCreateEvent           (Channel -> Effect DiscordM ())
              | ChannelUpdateEvent           (Channel -> Effect DiscordM ())
              | ChannelDeleteEvent           (Channel -> Effect DiscordM ())
              | GuildCreateEvent             (Guild -> Effect DiscordM ())
              | GuildUpdateEvent             (Guild -> Effect DiscordM ())
              | GuildDeleteEvent             (Guild -> Effect DiscordM ())
              | GuildBanAddEvent             (Member -> Effect DiscordM ())
              | GuildBanRemoveEvent          (Member -> Effect DiscordM ())
              | GuildEmojiUpdateEvent        (Object -> Effect DiscordM ())
              | GuildIntegrationsUpdateEvent (Object -> Effect DiscordM ())
              | GuildMemberAddEvent          (Member -> Effect DiscordM ())
              | GuildMemberRemoveEvent       (Member -> Effect DiscordM ())
              | GuildMemberUpdateEvent       (Member -> Effect DiscordM ())
              | GuildMemberChunkEvent        (Object -> Effect DiscordM ())
              | GuildRoleCreateEvent         (Object -> Effect DiscordM ())
              | GuildRoleUpdateEvent         (Object -> Effect DiscordM ())
              | GuildRoleDeleteEvent         (Object -> Effect DiscordM ())
              | MessageCreateEvent           (Message -> Effect DiscordM ())
              | MessageUpdateEvent           (Message -> Effect DiscordM ())
              | MessageDeleteEvent           (Object -> Effect DiscordM ())
              | MessageDeleteBulkEvent       (Object -> Effect DiscordM ())
              | PresenceUpdateEvent          (Object -> Effect DiscordM ())
              | TypingStartEvent             (Object -> Effect DiscordM ())
              | UserSettingsUpdateEvent      (Object -> Effect DiscordM ())
              | UserUpdateEvent              (Object -> Effect DiscordM ())
              | VoiceStateUpdateEvent        (Object -> Effect DiscordM ())
              | VoiceServerUpdateEvent       (Object -> Effect DiscordM ())
              | Event String                 (Object -> Effect DiscordM ())

  with :: (a -> Handle) -> a -> DiscordBot ()
  with f a = tell $ f a
  
  instance Monoid Handle where
    mempty = Null
    a `mappend` b = Misc ( \ev -> handle a ev <> handle b ev)

  handle :: Handle -> Event -> Effect DiscordM ()
  handle (Misc p)                         ev =                               p ev
  handle (ReadyEvent p)                   (D.Ready o)                   = p o
  handle (ResumedEvent p)                 (D.Resumed o)                 = p o
  handle (ChannelCreateEvent p)           (D.ChannelCreate o)           = p o
  handle (ChannelUpdateEvent p)           (D.ChannelUpdate o)           = p o
  handle (ChannelDeleteEvent p)           (D.ChannelDelete o)           = p o
  handle (GuildCreateEvent p)             (D.GuildCreate o)             = p o
  handle (GuildUpdateEvent p)             (D.GuildUpdate o)             = p o
  handle (GuildDeleteEvent p)             (D.GuildDelete o)             = p o
  handle (GuildBanAddEvent p)             (D.GuildBanAdd o)             = p o
  handle (GuildBanRemoveEvent p)          (D.GuildBanRemove o)          = p o
  handle (GuildEmojiUpdateEvent p)        (D.GuildEmojiUpdate o)        = p o
  handle (GuildIntegrationsUpdateEvent p) (D.GuildIntegrationsUpdate o) = p o
  handle (GuildMemberAddEvent p)          (D.GuildMemberAdd o)          = p o
  handle (GuildMemberRemoveEvent p)       (D.GuildMemberRemove o)       = p o
  handle (GuildMemberUpdateEvent p)       (D.GuildMemberUpdate o)       = p o
  handle (GuildMemberChunkEvent p)        (D.GuildMemberChunk o)        = p o
  handle (GuildRoleCreateEvent p)         (D.GuildRoleCreate o)         = p o
  handle (GuildRoleUpdateEvent p)         (D.GuildRoleUpdate o)         = p o
  handle (GuildRoleDeleteEvent p)         (D.GuildRoleDelete o)         = p o
  handle (MessageCreateEvent p)           (D.MessageCreate o)           = p o
  handle (MessageUpdateEvent p)           (D.MessageUpdate o)           = p o
  handle (MessageDeleteEvent p)           (D.MessageDelete o)           = p o
  handle (MessageDeleteBulkEvent p)       (D.MessageDeleteBulk o)       = p o
  handle (PresenceUpdateEvent p)          (D.PresenceUpdate o)          = p o
  handle (TypingStartEvent p)             (D.TypingStart o)             = p o
  handle (UserSettingsUpdateEvent p)      (D.UserSettingsUpdate o)      = p o
  handle (UserUpdateEvent p)              (D.UserUpdate o)              = p o
  handle (VoiceStateUpdateEvent p)        (D.VoiceStateUpdate o)        = p o
  handle (VoiceServerUpdateEvent p)       (D.VoiceServerUpdate o)       = p o
  handle (Event s p)                      (D.UnknownEvent v o)
    | s == v = p o
  handle _ ev = liftIO $ putStrLn $ "Miss event " ++ show ev
