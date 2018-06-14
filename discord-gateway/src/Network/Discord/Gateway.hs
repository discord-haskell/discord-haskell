{-# OPTIONS_HADDOCK prune, not-home #-}

-- | Provides logic code for interacting with the Discord websocket
--   gateway. Reallistically, this is probably lower level than most
--   people will need, and you should use Language.Discord.
module Network.Discord.Gateway where

import Control.Monad (void, forever)
import Control.Concurrent.Chan
import Control.Concurrent.MVar
import Control.Exception (try, SomeException)
import Control.Concurrent (threadDelay, forkIO)
import Control.Monad (forever)
import Data.Monoid ((<>))
import Data.IORef
import Data.Aeson
import qualified Data.Text as T
import qualified Data.ByteString.Lazy.Char8 as QL

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
      seqKey <- newIORef (-1)
      forkIO $ heartbeat conn interval seqKey log
      eventStream conn auth seqKey events log
      pure ()
  pure ()

logger :: Chan String -> IO ()
logger log = do
  putStrLn "starting the log"
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
heartbeat conn interval seqKey log = do
  writeChan log "starting the heartbeat"
  forever $ do
    num <- readIORef seqKey
    writeChan log ("heart " <> show num)
    send conn (Heartbeat num)
    writeChan log ("beat " <> show interval)
    threadDelay (interval * 1000)

setSequence :: IORef Integer -> Integer -> IO ()
setSequence key i = writeIORef key i

send :: Connection -> Payload -> IO ()
send conn payload =
  sendTextData conn (encode payload)

eventStream :: Connection -> Auth -> IORef Integer -> Chan Event -> Chan String -> IO ()
eventStream conn auth seqKey eventChan log = loop Start
  where
  loop :: GatewayState -> IO ()
  loop Start = do
    send conn (Identify auth False 50 (0, 1))
    loop Running
  loop Running = do
    writeChan log "Before step"
    eitherPayload <- try (step conn log)
    writeChan log "After step"
    writeChan log ("Payload - " <> show eitherPayload)
    case eitherPayload :: Either SomeException Payload of
      Left err -> loop InvalidReconnect
      Right d@(Dispatch _ sq _) -> do
        setSequence seqKey sq
        case parseDispatch d of
          Left reason -> writeChan log ("Discord-hs.Gateway.Dispatch - " <> reason)
          Right event -> do
            writeChan eventChan event
        loop Running
      Right (Heartbeat sq) -> do
        setSequence seqKey sq
        send conn (Heartbeat sq)
        loop Running
      Right (Reconnect)      -> loop InvalidReconnect
      Right (InvalidSession) -> loop Start
      Right (HeartbeatAck)   -> loop Running
      Right _ -> do
        writeChan log "Discord-hs.Gateway.Error - InvalidPacket"
        loop InvalidDead
  loop InvalidReconnect = writeChan log "should try and reconnect" >> loop InvalidDead
  loop InvalidDead      = writeChan log "Discord-hs.Gateway.Error - Bot died"

