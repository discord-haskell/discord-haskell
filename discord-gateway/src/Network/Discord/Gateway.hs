{-# OPTIONS_HADDOCK prune, not-home #-}

-- | Provides logic code for interacting with the Discord websocket
--   gateway. Reallistically, this is probably lower level than most
--   people will need, and you should use Language.Discord.
module Network.Discord.Gateway where

import Control.Monad (void, forever)
import Control.Monad.Random
import Control.Concurrent.Chan
import Control.Concurrent.MVar
import Control.Exception (try, SomeException)
import Control.Concurrent (threadDelay, forkIO)
import Control.Monad (forever)
import Data.Monoid ((<>))
import Data.IORef
import Data.Aeson
import Data.Aeson.Types
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as Q
import qualified Data.ByteString.Lazy.Char8 as QL

import Wuss
import Network.WebSockets hiding (send)

import Network.Discord.Types

data GatewayState = Start
                  | Running
                  | InvalidReconnect
                  | InvalidDead

data ConnLoopState = ConnStart
                   | ConnClosed
                   | ConnReconnect Q.ByteString String String

data ConnectionData = ConnData { connection :: Connection
                               , connSessionID :: IORef String
                               , connAuth :: Auth
                               , connChan :: (Chan Event)
                               , heartbeatInteval :: Int
                               }


newSocket :: DiscordAuth -> Chan Event -> IO ()
newSocket (DiscordAuth auth _) events = do
  log <- newChan
  forkIO (logger log)
  forkIO (connectionLoop auth ConnStart events log)
  pure ()

connectionLoop s auth events log = loop
  where
  loop = case s of
    (ConnStart) -> do
        runSecureClient "gateway.discord.gg" 443 "/?v=6&encoding=json" $ \conn -> do
          Hello interval <- step conn log
          resp <- startEventStream events auth interval (-1) log
          loop resp
    (ConnClosed) -> writeChan log "ConnClosed"
    (ConnReconnect tok seshID seqID interval) -> do
        startEventStream events auth (pure interval) seqID log
        runSecureClient "gateway.discord.gg" 443 "/?v=6&encoding=json" $ \conn -> do
          send conn (Resume tok seshID seqID)
          writeChan log "Resuming???"
          eitherPayload <- step conn log
          case eitherPayload of
            Left _ -> connectionLoop ConnClosed
            Right InvalidSession -> do t <- getRandomR (1,5)
                                       writeChan log ("Failed ot connect. waiting:" <> show t)
                                       threadDelay (t * 10^6)
                                       loop ConnStart
            Right d@(Dispatch _) -> do case parseDispatch d of
                                          Right event -> writeChan events event
                                          _ -> pure ()
                                       resp <- startEventStream events auth interval seqKey log
                                       loop resp
            Right payload -> do writeChan log ("Why did they send a: " <> show payload)
                                loop ConnClosed

startEventStream events auth interval seqN log = do
    seqKey <- newIORef seqN
    heart <- forkIO $ heartbeat conn interval seqKey log
    sID <- newIORef ""
    resp <- eventStream (ConnData conn sID auth events interval) seqKey log
    killThread heart
    pure resp

logger :: Chan String -> IO ()
logger log = do
  putStrLn "starting the log"
  forever $ readChan log >>= putStrLn

step :: Connection -> Chan String -> IO (Either SomeException Payload)
step connection log = try $ do
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

eventStream :: ConnectionData -> IORef Integer -> Chan String -> IO ConnLoopState
eventStream (ConnData conn sID auth eventChan interval) seqKey log = loop Start
  where
  loop :: GatewayState -> IO ()
  loop Start = do
    send conn (Identify auth False 50 (0, 1))
    loop Running
  loop Running = do
    writeChan log "Before step"
    eitherPayload <- step conn log
    writeChan log "After step"
    writeChan log ("Payload - " <> show eitherPayload)
    case eitherPayload :: Either SomeException Payload of
      Left _ -> loop InvalidReconnect
      Right (Dispatch obj sq name) -> do
        setSequence seqKey sq
        case parseDispatch (Dispatch obj sq name) of
          Left reason -> writeChan log ("Discord-hs.Gateway.Dispatch - " <> reason)
          Right event -> do
            writeChan eventChan event
            case (name, parseMaybe (.: "session_id") obj) of
              ("READY", Just sesh) -> writeIORef sID sesh
              _                    -> pure ()
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
  loop InvalidReconnect = do writeChan log "should try and reconnect"
                             let (Bot tok) = auth
                             seshID <- readIORef sID
                             seqID <- readIORef seqKey
                             pure (ConnReconnect tok seshID seqID interval)

  loop InvalidDead      = do writeChan log "Discord-hs.Gateway.Error - Bot died"
                             pure ConnClosed
