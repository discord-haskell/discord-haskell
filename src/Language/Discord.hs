{-# LANGUAGE TypeFamilies, ExistentialQuantification, RankNTypes, MultiParamTypeClasses #-}
module Language.Discord where
  import Pipes.Core
  import Network.Discord as D hiding (Payload)

  data BotClient = BotClient Auth
  instance D.Client BotClient where
    getAuth (BotClient auth) = auth

  runBot :: Auth -> DiscordBot () -> IO ()
  runBot auth bot = do
    gateway <- getGateway
    runWebsocket gateway (BotClient auth) $ do
      DiscordState {getWebSocket=ws} <- get
      (eventCore ~> runEvent bot) ws

  data DiscordBot a = Bot {runEvent :: Event -> Effect DiscordM a} | PassEvent

  instance Functor DiscordBot where
    fmap _ PassEvent = PassEvent
    f `fmap` a = Bot $ fmap f . runEvent a

  instance Applicative DiscordBot where
    pure = Bot . const . return
    f <*> a = Bot $ \ev -> runEvent f ev <*> runEvent a ev
