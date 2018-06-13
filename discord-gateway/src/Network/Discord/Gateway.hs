{-# OPTIONS_HADDOCK prune, not-home #-}

-- | Provides logic code for interacting with the Discord websocket
--   gateway. Reallistically, this is probably lower level than most
--   people will need, and you should use Language.Discord.
module Network.Discord.Gateway where

import Control.Monad (void, forever)
import Control.Concurrent.Chan
import Control.Concurrent (threadDelay, forkIO)
import Control.Monad (forever)
import Data.Monoid ((<>))
import Data.IORef
import Data.Aeson

import Wuss
import Network.WebSockets hiding (send)

import Network.Discord.Types

data GatewayState
  = Start
  | Running
  | InvalidReconnect
  | InvalidDead

newSocket :: DiscordAuth -> Chan Event -> IO ()
newSocket (DiscordAuth auth _) events = do
  log <- newChan
  forkIO (logger log)
  forkIO $
    runSecureClient "gateway.discord.gg" 443 "/?v=6&encoding=json" $ \conn -> do
      Hello interval <- step conn log
      sequenceKey <- newIORef (-1)
      forkIO $ heartbeat conn interval sequenceKey log
      eventStream conn auth sequenceKey events log
      pure ()
  pure ()

logger :: Chan String -> IO ()
logger log = do
  putStrLn "staring the log"
  forever $ readChan log >>= putStrLn

step :: Connection -> Chan String -> IO Payload
step connection log = do
  msg' <- receiveData connection
  case eitherDecode msg' of
    Right msg -> return msg
    Left  err -> do writeChan log ("Discord-hs.Gateway.Parse" <> err)
                    writeChan log ("Discord-hs.Gateway.Raw" <> show msg')
                    return (ParseError err)

heartbeat :: Connection -> Int -> IORef Integer -> Chan String -> IO ()
heartbeat conn interval sequenceKey log = do
  writeChan log "starting heartbeating"
  forever $ do
    num <- readIORef sequenceKey
    writeChan log ("heart " <> show num)
    send conn (Heartbeat num)
    writeChan log ("beat " <> show interval)
    threadDelay (interval * 1000)

setSequence :: IORef Integer -> Integer -> IO ()
setSequence = writeIORef

send :: Connection -> Payload -> IO ()
send conn payload =
  sendTextData conn (encode payload)

eventStream :: Connection -> Auth -> IORef Integer -> Chan Event -> Chan String
    -> IO ()
eventStream conn auth sequenceKey eventChan log = loop Start
  where
  loop :: GatewayState -> IO ()
  loop Start = do
    send conn (Identify auth False 50 (0, 1))
    loop Running
  loop Running = do
    writeChan log "Before step"
    payload <- step conn log
    writeChan log "after step"
    writeChan log $ "Payload - " <> show payload
    case payload of
      Dispatch _ sq _ -> do
        setSequence sequenceKey sq
        case parseDispatch payload of
          Left reason -> writeChan log ("Discord-hs.Gateway.Dispatch - " <> reason)
          Right event -> do
            writeChan eventChan event
        loop Running
      Heartbeat sq -> do
        setSequence sequenceKey sq
        send conn (Heartbeat sq)
        loop Running
      Reconnect      -> loop InvalidReconnect
      InvalidSession -> loop Start
      HeartbeatAck   -> loop Running
      _              -> do
        writeChan log "Discord-hs.Gateway.Error - InvalidPacket"
        loop InvalidDead
  loop InvalidReconnect = writeChan log "invalid recon" >> loop InvalidDead
  loop InvalidDead      = writeChan log "Discord-hs.Gateway.Error - Bot died"

