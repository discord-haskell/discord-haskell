{-# LANGUAGE OverloadedStrings, FlexibleContexts, RankNTypes #-}
{-# OPTIONS_HADDOCK prune, not-home #-}
-- | Provides logic code for interacting with the Discord websocket
--   gateway. Reallistically, this is probably lower level than most
--   people will need, and you should use Language.Discord.
module Network.Discord.Gateway where
  import Control.Concurrent (forkIO, threadDelay)
  import Control.Monad.State
  import Data.ByteString.Lazy.Char8 (unpack)

  import Control.Concurrent.STM
  import Data.Aeson
  import Data.Aeson.Types
  import Network.WebSockets
  import Network.URL
  import Pipes
  import System.Log.Logger
  import Wuss

  import Network.Discord.Types

  -- | Turn a websocket Connection into a Pipes Producer
  --   (data source)
  makeWebsocketSource :: (FromJSON a, MonadIO m)
    => Connection -> Producer a m ()
  makeWebsocketSource conn = forever $ do
    msg' <- lift . liftIO $ receiveData conn
    case eitherDecode msg' of
      Right msg -> yield $ msg
      Left  err -> liftIO $
        errorM "Discord-hs.Gateway.Parse" err
          >> infoM "Discord-hs.Gateway.Raw" (unpack msg')

  -- | Starts a websocket connection that allows you to handle Discord events.
  runWebsocket :: (Client c)
    => URL -> c -> Effect DiscordM a -> IO a
  runWebsocket (URL (Absolute h) path _) client inner = do
    rl <- newTVarIO []
    runSecureClient (host h) 443 (path++"/?v=6")
      $ \conn -> evalDiscordM (runEffect inner)
        (DiscordState Create client conn undefined rl)
  runWebsocket _ _ _ = mzero

  -- | Spawn a heartbeat thread
  heartbeat :: Int -> Connection -> TMVar Integer -> IO ()
  heartbeat interval conn sq = void . forkIO . forever $ do
    seqNum <- atomically $ readTMVar sq
    sendTextData conn $ Heartbeat seqNum
    threadDelay $ interval * 1000
  
  -- | Turn a websocket data source into an 'Event' data
  --   source
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
          Right a     -> yield  $ Ready a
          Left reason -> liftIO $ errorM "Discord-hs.Gateway.Dispatch" reason
        put st {getState=Running}
      Running          -> do
        pl <- await
        case pl of
          Dispatch _ sq _ -> do
            liftIO . atomically $ tryTakeTMVar (getSequenceNum st)
              >> putTMVar (getSequenceNum st) sq
            case parseDispatch pl of
              Left reason   -> liftIO $ errorM "Discord-hs.Gateway.Dispatch" reason
              Right payload -> yield payload
          Heartbeat sq    -> do
            liftIO . atomically $ tryTakeTMVar (getSequenceNum st)
              >> putTMVar (getSequenceNum st) sq
            liftIO . sendTextData ws $ Heartbeat sq
          Reconnect       -> put st {getState=InvalidReconnect}
          InvalidSession  -> put st {getState=Start}
          HeartbeatAck    -> liftIO $ infoM "Discord-hs.Gateway.Heartbeat" "HeartbeatAck"
          _               -> do
            liftIO $ errorM "Discord-hs.Gateway.Error" "InvalidPacket"
            put st {getState=InvalidDead}
      InvalidReconnect -> put st {getState=InvalidDead}
      InvalidDead      -> liftIO $ errorM "Discord-hs.Gateway.Error" "BotDied"
  
  -- | Utility function providing core functionality by converting a Websocket
  --   'Connection' to a stream of gateway 'Event's
  eventCore :: Connection -> Producer Event DiscordM ()
  eventCore conn = makeWebsocketSource conn >-> makeEvents

