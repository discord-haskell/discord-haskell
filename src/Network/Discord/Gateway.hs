{-# LANGUAGE OverloadedStrings, FlexibleContexts, RankNTypes #-}
{-# OPTIONS_HADDOCK prune, not-home #-}
module Network.Discord.Gateway where
  import Control.Concurrent (forkIO, threadDelay)
  import Control.Monad.State
  import System.Mem

  import Wuss
  import Data.Aeson
  import Data.Aeson.Types
  import Network.WebSockets
  import Network.URL
  import Control.Concurrent.STM
  import Pipes
  import System.Log.Logger

  import Network.Discord.Types


  makeWebsocketSource :: (WebSocketsData a, MonadIO m)
    => Connection -> Producer a m ()
  makeWebsocketSource conn = forever $ do
    msg <- lift . liftIO $ receiveData conn
    yield msg

  -- | Starts a websocket connection that allows you to handle Discord events.
  runWebsocket :: (Client c)
    => URL -> c -> Effect DiscordM a -> IO a
  runWebsocket (URL (Absolute h) path _) client inner =
    runSecureClient (host h) 443 (path++"/?v=6")
      $ \conn -> evalStateT (runEffect inner)
        (DiscordState Create client conn undefined [])
  runWebsocket _ _ _ = mzero

  -- | Spawn a heartbeat thread
  heartbeat :: Int -> Connection -> TMVar Integer -> IO ()
  heartbeat interval conn sq = void . forkIO . forever $ do
    seqNum <- atomically $ readTMVar sq
    sendTextData conn $ Heartbeat seqNum
    threadDelay $ interval * 1000
    performGC

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
          Left reason -> liftIO $ errorM "Discord-hs.Gateway" reason
        put st {getState=Running}
      Running          -> do
        pl <- await
        case pl of
          Dispatch _ sq _ -> do
            liftIO . atomically $ tryTakeTMVar (getSequenceNum st)
              >> putTMVar (getSequenceNum st) sq
            case parseDispatch pl of
              Left reason   -> liftIO $ errorM "Discord-hs.Gateway" reason
              Right payload -> yield payload
          Heartbeat sq    -> do
            liftIO . atomically $ tryTakeTMVar (getSequenceNum st)
              >> putTMVar (getSequenceNum st) sq
            liftIO . sendTextData ws $ Heartbeat sq
          Reconnect       -> put st {getState=InvalidReconnect}
          InvalidSession  -> put st {getState=Start}
          HeartbeatAck    -> liftIO $ infoM "Discord-hs.Gateway" "Heartbeat Ack"
          _               -> do
            liftIO $ putStrLn "Invalid Packet"
            put st {getState=InvalidDead}
      InvalidReconnect -> put st {getState=InvalidDead}
      InvalidDead      -> liftIO $ errorM "Discord-hs.Gateway" "Bot died"

  eventCore :: Connection -> Producer Event DiscordM ()
  eventCore conn = makeWebsocketSource conn >-> makeEvents
