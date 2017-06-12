{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, FlexibleInstances #-}
{-# OPTIONS_HADDOCK prune, not-home #-}
-- | Provides logic code for interacting with the Discord websocket
--   gateway. Reallistically, this is probably lower level than most
--   people will need, and you should use Language.Discord.
module Network.Discord.Gateway where
  import Control.Concurrent (threadDelay)
  import Control.Monad (forever)
  import Control.Monad.IO.Class (liftIO)
  import Data.Maybe (fromJust)

  import Data.Aeson
  import Network.WebSockets hiding (send)
  import Network.URL
  import System.Log.Logger
  import Wuss

  import Network.Discord.Types

  data GatewayState
    = Create
    | Start
    | Running
    | InvalidReconnect
    | InvalidDead

  class DiscordAuth m => DiscordGate m where
    data VaultKey m a
    
    type Vault m :: * -> *
    -- | Retrieve a value from the store, blocking if the store is empty
    get :: Vault m a -> m a
    -- | Place a value into the store, discarding the old value if present.
    put  :: Vault m a -> a -> m ()

    sequenceKey :: VaultKey m Integer
    storeFor :: VaultKey m a -> m ((Vault m) a)

    connection   :: m Connection
    feed :: m () -> Event -> m ()

    run  :: m () -> Connection -> IO ()
    fork :: m () -> m ()

  runGateway :: DiscordGate m => URL -> m () -> IO ()
  runGateway (URL (Absolute h) path _) client =
    runSecureClient (host h) 443 (path ++ "/?v=6")
      $ run client
  runGateway _ _ = return ()

  send :: DiscordGate m => Payload -> m ()
  send payload = do
    conn <- connection
    liftIO . sendTextData conn $ encode payload

  heartbeat :: DiscordGate m => Int -> m ()
  heartbeat interval = fork . forever $ do
    seqNum <- Heartbeat <$> (get =<< storeFor sequenceKey)
    send seqNum
    liftIO $ threadDelay (interval * 1000)

  setSequence :: DiscordGate m => Integer -> m ()
  setSequence sq = do
    seqNum <- storeFor sequenceKey
    put seqNum sq

  eventStream :: DiscordGate m => GatewayState -> m () -> m ()
  eventStream Create m = do
    Hello interval <- step
    heartbeat interval
    eventStream Start m
  eventStream Start m = do
    creds <- auth
    send $ Identify creds False 50 (0, 1)
    eventStream Running m
  eventStream Running m = do
    payload <- step
    case payload of
      Dispatch _ sq _ -> do
        setSequence sq
        case parseDispatch payload of
          Left reason -> liftIO $ errorM "Discord-hs.Gateway.Dispatch" reason
          Right event -> do
            liftIO $ print event
            feed m event
        liftIO $ putStrLn "Stepping app"
        eventStream Running m
      Heartbeat sq -> do
        setSequence sq
        send $ Heartbeat sq
        eventStream Running m
      Reconnect      -> eventStream InvalidReconnect m
      InvalidSession -> eventStream Start m
      HeartbeatAck   -> eventStream Running m
      _              -> do
        liftIO $ errorM "Discord-hs.Gateway.Error" "InvalidPacket"
        liftIO $ putStrLn "DYING RIP ME"
        eventStream InvalidDead m
  eventStream InvalidReconnect m = eventStream InvalidDead m
  eventStream InvalidDead      _ = liftIO $ errorM "Discord-hs.Gateway.Error" "Bot died"

  step :: DiscordGate m => m Payload
  step = do
    conn <- connection
    liftIO $ putStrLn "Waiting for data"
    msg' <- liftIO $ receiveData conn
    liftIO $ putStrLn "Got data"
    case eitherDecode msg' of
      Right msg -> return msg
      Left  err -> 
        ( liftIO
          $  errorM "Discord-hs.Gateway.Parse" err
          >> infoM "Discord-hs.Gateway.Raw" (show msg')
        ) >> (return $ ParseError err)
  
  gatewayUrl :: URL
  gatewayUrl = fromJust $ importURL "wss://gateway.discord.gg"
