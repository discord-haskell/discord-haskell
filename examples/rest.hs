{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
import Data.Text
import Pipes

import Network.Discord

main :: IO ()
main = 

data BotClient = BotClient Auth
instance D.Client BotClient where
    getAuth (BotClient auth) = auth

runBot :: Auth -> DiscordBot () -> IO ()
runBot auth bot = do
    runDiscord (BotClient auth) $ do
      DiscordState {getWebSocket=ws} <- get
      (eventCore ~> (handle $ execWriter bot)) ws

type DiscordBot a = Writer Handle a

runDiscordM :: Effect DiscordM a -> IO a
runDiscordM inner = evalStateT (runEffect inner) (DiscordState Create client conn undefined [])

makeEvents :: Pipe Payload Event DiscordM a
makeEvents = forever $ do
  st@(DiscordState dState client ws _ _) <- get
  case dState of
    Create -> do
      Hello interval <- await
      sq <- liftIO . atomically $ newEmptyTMVar
      put st {getState=Start, getSequenceNum=sq}
      liftIO $ heartbeat interval ws sq
    Start  -> do
      liftIO $ sendTextData ws
        (Identify (getAuth client) False 50 (0, 1))
      Dispatch o sq "READY" <- await
      liftIO . atomically $ putTMVar (getSequenceNum st) sq
      case parseEither parseJSON $ Object o of
        Right a -> yield $ Ready a
        Left reason -> liftIO $ putStrLn reason
      put st {getState=Running}
    Running          -> do
      pl <- await
      case pl of
        Dispatch _ sq _ -> do
          liftIO . atomically $ tryTakeTMVar (getSequenceNum st)
            >> putTMVar (getSequenceNum st) sq
          yield $ parseDispatch pl
        Heartbeat sq    -> do
          liftIO . atomically $ tryTakeTMVar (getSequenceNum st)
            >> putTMVar (getSequenceNum st) sq
          liftIO . sendTextData ws $ Heartbeat sq
        Reconnect       -> put st {getState=InvalidReconnect}
        InvalidSession  -> put st {getState=Start}
        HeartbeatAck    -> liftIO $ putStrLn "Heartbeat Ack"
        _               -> do
          liftIO $ putStrLn "Invalid Packet"
          put st {getState=InvalidDead}
    InvalidReconnect -> put st {getState=InvalidDead}
    InvalidDead      -> liftIO $ putStrLn "Bot died"

