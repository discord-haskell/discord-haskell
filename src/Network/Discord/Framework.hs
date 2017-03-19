-- | Provides a convenience framework for writing Discord bots without dealing with Pipes
module Network.Discord.Framework where
  import Control.Concurrent
  import Control.Monad.Writer
  import Data.Proxy

  import Control.Concurrent.STM
  import Control.Monad.State (get)
  import Data.Aeson (Object)
  import Pipes ((~>))
  import Pipes.Core hiding (Proxy)
  import System.Log.Logger

  import Network.Discord.Gateway as D
  import Network.Discord.Rest    as D
  import Network.Discord.Types   as D

  -- | Isolated state representation for use with async event handling
  asyncState :: D.Client a => a -> Effect DiscordM DiscordState
  asyncState client = do
    DiscordState { getRateLimits = limits } <- get
    return $ DiscordState
      Running
      client
      undefined
      undefined
      limits

  -- | Basic client implementation. Most likely suitable for most bots.
  data BotClient = BotClient Auth
  instance D.Client BotClient where
    getAuth (BotClient auth) = auth

  -- | This should be the entrypoint for most Discord bots.
  runBot :: Auth -> DiscordBot BotClient () -> IO ()
  runBot auth bot = runBotWith (BotClient auth) bot

  -- | A variant of 'runBot' which allows the user to specify a custom client implementation.
  runBotWith :: D.Client a => a -> DiscordBot a () -> IO ()
  runBotWith client bot = do
    gateway <- getGateway
    atomically $ writeTVar getTMClient client
    runWebsocket gateway client $ do
      DiscordState {getWebSocket=ws} <- get
      (eventCore ~> (handle $ execWriter bot)) ws

  -- | Utility function to split event handlers into a seperate thread
  runAsync :: D.Client client => Proxy client -> Effect DiscordM () -> Effect DiscordM ()
  runAsync c effect = do
      client <- liftIO . atomically $ getSTMClient c
      st <- asyncState client
      liftIO . void $ forkFinally
        (execDiscordM (runEffect effect) st)
        finish
    where
      finish (Right DiscordState{getClient = st}) = atomically $ mergeClient st
      finish (Left err) = errorM "Language.Discord.Events" $ show err

  -- | Monad to compose event handlers
  type DiscordBot c a = Writer (Handle c) a

  -- | Event handlers for 'Gateway' events. These correspond to events listed in
  --   'Event'
  data D.Client c => Handle c = Null
                              | Misc                         (Event   -> Effect DiscordM ())
                              | ReadyEvent                   (Init    -> Effect DiscordM ())
                              | ResumedEvent                 (Object  -> Effect DiscordM ())
                              | ChannelCreateEvent           (Channel -> Effect DiscordM ())
                              | ChannelUpdateEvent           (Channel -> Effect DiscordM ())
                              | ChannelDeleteEvent           (Channel -> Effect DiscordM ())
                              | GuildCreateEvent             (Guild   -> Effect DiscordM ())
                              | GuildUpdateEvent             (Guild   -> Effect DiscordM ())
                              | GuildDeleteEvent             (Guild   -> Effect DiscordM ())
                              | GuildBanAddEvent             (Member  -> Effect DiscordM ())
                              | GuildBanRemoveEvent          (Member  -> Effect DiscordM ())
                              | GuildEmojiUpdateEvent        (Object  -> Effect DiscordM ())
                              | GuildIntegrationsUpdateEvent (Object  -> Effect DiscordM ())
                              | GuildMemberAddEvent          (Member  -> Effect DiscordM ())
                              | GuildMemberRemoveEvent       (Member  -> Effect DiscordM ())
                              | GuildMemberUpdateEvent       (Member  -> Effect DiscordM ())
                              | GuildMemberChunkEvent        (Object  -> Effect DiscordM ())
                              | GuildRoleCreateEvent         (Object  -> Effect DiscordM ())
                              | GuildRoleUpdateEvent         (Object  -> Effect DiscordM ())
                              | GuildRoleDeleteEvent         (Object  -> Effect DiscordM ())
                              | MessageCreateEvent           (Message -> Effect DiscordM ())
                              | MessageUpdateEvent           (Message -> Effect DiscordM ())
                              | MessageDeleteEvent           (Object  -> Effect DiscordM ())
                              | MessageDeleteBulkEvent       (Object  -> Effect DiscordM ())
                              | PresenceUpdateEvent          (Object  -> Effect DiscordM ())
                              | TypingStartEvent             (Object  -> Effect DiscordM ())
                              | UserSettingsUpdateEvent      (Object  -> Effect DiscordM ())
                              | UserUpdateEvent              (Object  -> Effect DiscordM ())
                              | VoiceStateUpdateEvent        (Object  -> Effect DiscordM ())
                              | VoiceServerUpdateEvent       (Object  -> Effect DiscordM ())
                              | Event String                 (Object  -> Effect DiscordM ())

  -- | Provides a typehint for the correct 'D.Client' given an Event 'Handle'
  clientProxy   :: Handle c -> Proxy c
  clientProxy _ = Proxy

  -- | Register an Event 'Handle' in the 'DiscordBot' monad
  with :: D.Client c => (a -> Handle c) -> a -> DiscordBot c ()
  with f a = tell $ f a
  
  instance D.Client c => Monoid (Handle c) where
    mempty = Null
    a `mappend` b = Misc (\ev -> handle a ev <> handle b ev)

  -- | Asynchronously run an Event 'Handle' against a Gateway 'Event'
  handle :: D.Client a => Handle a -> Event -> Effect DiscordM ()
  handle a@(Misc p)                          ev                           
    = runAsync (clientProxy a) $ p ev
  handle a@(ReadyEvent p)                   (D.Ready o)                   
    = runAsync (clientProxy a) $ p o
  handle a@(ResumedEvent p)                 (D.Resumed o)                 
    = runAsync (clientProxy a) $ p o
  handle a@(ChannelCreateEvent p)           (D.ChannelCreate o)           
    = runAsync (clientProxy a) $ p o
  handle a@(ChannelUpdateEvent p)           (D.ChannelUpdate o)           
    = runAsync (clientProxy a) $ p o
  handle a@(ChannelDeleteEvent p)           (D.ChannelDelete o)           
    = runAsync (clientProxy a) $ p o
  handle a@(GuildCreateEvent p)             (D.GuildCreate o)             
    = runAsync (clientProxy a) $ p o
  handle a@(GuildUpdateEvent p)             (D.GuildUpdate o)             
    = runAsync (clientProxy a) $ p o
  handle a@(GuildDeleteEvent p)             (D.GuildDelete o)             
    = runAsync (clientProxy a) $ p o
  handle a@(GuildBanAddEvent p)             (D.GuildBanAdd o)             
    = runAsync (clientProxy a) $ p o
  handle a@(GuildBanRemoveEvent p)          (D.GuildBanRemove o)          
    = runAsync (clientProxy a) $ p o
  handle a@(GuildEmojiUpdateEvent p)        (D.GuildEmojiUpdate o)        
    = runAsync (clientProxy a) $ p o
  handle a@(GuildIntegrationsUpdateEvent p) (D.GuildIntegrationsUpdate o) 
    = runAsync (clientProxy a) $ p o
  handle a@(GuildMemberAddEvent p)          (D.GuildMemberAdd o)          
    = runAsync (clientProxy a) $ p o
  handle a@(GuildMemberRemoveEvent p)       (D.GuildMemberRemove o)      
    = runAsync (clientProxy a) $ p o
  handle a@(GuildMemberUpdateEvent p)       (D.GuildMemberUpdate o)      
    = runAsync (clientProxy a) $ p o
  handle a@(GuildMemberChunkEvent p)        (D.GuildMemberChunk o)       
    = runAsync (clientProxy a) $ p o
  handle a@(GuildRoleCreateEvent p)         (D.GuildRoleCreate o)        
    = runAsync (clientProxy a) $ p o
  handle a@(GuildRoleUpdateEvent p)         (D.GuildRoleUpdate o)        
    = runAsync (clientProxy a) $ p o
  handle a@(GuildRoleDeleteEvent p)         (D.GuildRoleDelete o)        
    = runAsync (clientProxy a) $ p o
  handle a@(MessageCreateEvent p)           (D.MessageCreate o)          
    = runAsync (clientProxy a) $ p o
  handle a@(MessageUpdateEvent p)           (D.MessageUpdate o)          
    = runAsync (clientProxy a) $ p o
  handle a@(MessageDeleteEvent p)           (D.MessageDelete o)          
    = runAsync (clientProxy a) $ p o
  handle a@(MessageDeleteBulkEvent p)       (D.MessageDeleteBulk o)      
    = runAsync (clientProxy a) $ p o
  handle a@(PresenceUpdateEvent p)          (D.PresenceUpdate o)         
    = runAsync (clientProxy a) $ p o
  handle a@(TypingStartEvent p)             (D.TypingStart o)            
    = runAsync (clientProxy a) $ p o
  handle a@(UserSettingsUpdateEvent p)      (D.UserSettingsUpdate o)     
    = runAsync (clientProxy a) $ p o
  handle a@(UserUpdateEvent p)              (D.UserUpdate o)             
    = runAsync (clientProxy a) $ p o
  handle a@(VoiceStateUpdateEvent p)        (D.VoiceStateUpdate o)       
    = runAsync (clientProxy a) $ p o
  handle a@(VoiceServerUpdateEvent p)       (D.VoiceServerUpdate o)      
    = runAsync (clientProxy a) $ p o
  handle a@(Event s p)                      (D.UnknownEvent v o)
    | s == v = runAsync (clientProxy a) $ p o
  handle _ ev = liftIO $ debugM "Discord-hs.Language.Events" $ show ev
