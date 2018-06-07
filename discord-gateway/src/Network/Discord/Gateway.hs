{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, FlexibleInstances #-}
{-# OPTIONS_HADDOCK prune, not-home #-}
-- | Provides logic code for interacting with the Discord websocket
--   gateway. Reallistically, this is probably lower level than most
--   people will need, and you should use Language.Discord.
module Network.Discord.Gateway where

import Control.Concurrent.Chan
import Control.Concurrent (threadDelay, forkIO)
import Control.Monad (forever)
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (fromJust)
import Data.IORef
import Data.Aeson
import Network.URL
import Wuss
import Network.WebSockets hiding (send)

import Network.Discord.Types

data GatewayState
  = Start
  | Running
  | InvalidReconnect
  | InvalidDead

-- class DiscordAuth m => DiscordGate m where
--   data VaultKey m a
--
--   type Vault m :: * -> *
--   -- | Retrieve a value from the store, blocking if the store is empty
--   get :: Vault m a -> m a
--   -- | Place a value into the store, discarding the old value if present.
--   put  :: Vault m a -> a -> m ()
--
--   sequenceKey :: VaultKey m Integer
--   storeFor :: VaultKey m a -> m ((Vault m) a)
--
--   connection   :: m Connection
--   feed :: m () -> Event -> m ()
--
--   run  :: m () -> Connection -> IO ()
--   fork :: m () -> m ()

newSocket :: DiscordAuth -> IO ()
newSocket (DiscordAuth auth _) = do
  runSecureClient "gateway.discord.gg" 433 "/?v=6" $ \connection -> do
      Hello interval <- step connection
      sequenceKey <- newIORef
      forkIO $ heartbeat interval sequenceKey
      eventStream Start m
  print "Exiting socket"
  return ()

step :: Connection -> IO Payload
step connection = do
  putStrLn "Waiting for data"
  msg' <- receiveData connection
  putStrLn "Got data"
  case eitherDecode msg' of
    Right msg -> return msg
    Left  err -> do putStrLn ("Discord-hs.Gateway.Parse" <> err)
                    putStrLn ("Discord-hs.Gateway.Raw" <> show msg')
                    return (ParseError err)

heartbeat :: Int -> IORef Integer -> IO ()
heartbeat interval sequenceKey = forever $ do
  num <- readIORef sequenceKey
  send (Heartbeat num)
  threadDelay (interval * 1000)

setSequence :: IORef Integer -> Integer -> IO ()
setSequence = writeIORef

send :: Connection -> Payload -> IO ()
send conn payload = sendTextData conn (encode payload)

eventStream :: Connection -> Auth -> IORef Integer -> Chan Payload -> IO ()
eventStream conn auth sequenceKey eventChan = loop Start
  where
  loop :: GatewayState -> IO ()
  loop start = do
    send conn $ Identify auth False 50 (0, 1)
    loop Running
  loop Running = do
    payload <- step conn
    case payload of
      Dispatch _ sq _ -> do
        setSequence sequenceKey sq
        case parseDispatch payload of
          Left reason -> liftIO $ errorM "Discord-hs.Gateway.Dispatch" reason
          Right event -> do
            print event
            writeChan eventChan event
        putStrLn "Stepping app"
        loop Running
      Heartbeat sq -> do
        setSequence sequenceKey sq
        send $ Heartbeat sq
        eventStream Running m
      Reconnect      -> loop InvalidReconnect
      InvalidSession -> loop Start
      HeartbeatAck   -> loop Running
      _              -> do
        errorM "Discord-hs.Gateway.Error" "InvalidPacket"
        putStrLn "DYING RIP ME"
        eventStream InvalidDead m
  loop InvalidReconnect = loop InvalidDead
  loop InvalidDead      = errorM "Discord-hs.Gateway.Error" "Bot died"

