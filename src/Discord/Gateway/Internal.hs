{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_HADDOCK prune, not-home #-}

-- | Provides logic code for interacting with the Discord websocket
--   gateway. Realistically, this is probably lower level than most
--   people will need
module Discord.Gateway.Internal where

import Prelude hiding (log)

import Control.Monad (forever, (<=<))
import Control.Monad.Random (getRandomR)
import Control.Concurrent.Chan
import Control.Exception.Safe (try, finally, handle, SomeException)
import Control.Concurrent (threadDelay, killThread, forkIO)
import Data.Monoid ((<>))
import Data.IORef
import Data.Aeson (eitherDecode, encode)
import qualified Data.ByteString.Char8 as Q
import qualified Data.ByteString.Lazy.Char8 as QL

import Wuss (runSecureClient)
import Network.WebSockets (ConnectionException(..), Connection, receiveData, sendTextData)

import Discord.Types

data GatewayState = Running
                  | InvalidReconnect
                  | InvalidDead

data ConnLoopState = ConnStart
                   | ConnClosed
                   | ConnReconnect Q.ByteString String Integer
  deriving Show

data ConnectionData = ConnData { connection :: Connection
                               , connSessionID :: String
                               , connAuth :: Q.ByteString
                               , connChan :: (Chan Event)
                               }

connectionLoop :: Auth -> Chan Event -> Chan String -> IO ()
connectionLoop auth events log = loop ConnStart
  where
  loop :: ConnLoopState -> IO ()
  loop s = do
    writeChan log ("conn loop: " <> show s)
    case s of
      (ConnClosed) -> writeChan log "Conn Closed"
      (ConnStart) -> do
          loop <=< runSecureClient "gateway.discord.gg" 443 "/?v=6&encoding=json" $ \conn -> do
            msg <- step conn log
            case msg of
              Right (Hello interval) -> do
                send conn (Identify auth False 50 (0, 1)) log
                msg2 <- step conn log
                case msg2 of
                  Right (Dispatch (Ready (Init _ _ _ _ seshID)) _) ->
                    startEventStream conn events auth seshID interval 0 log
                  _ -> writeChan log ("received: " <> show msg2) >> pure ConnClosed
              _ -> writeChan log ("received: " <> show msg) >> pure ConnClosed
      (ConnReconnect tok seshID seqID) -> do
          loop <=< runSecureClient "gateway.discord.gg" 443 "/?v=6&encoding=json" $ \conn -> do
            send conn (Resume tok seshID seqID) log
            writeChan log "Resuming???"
            eitherPayload <- step conn log
            case eitherPayload of
              Right (Hello interval) -> startEventStream conn events auth seshID interval seqID log
              Right InvalidSession -> do t <- getRandomR (1,5)
                                         writeChan log ("Failed to connect. waiting:" <> show t)
                                         threadDelay (t * 10^6)
                                         pure ConnStart
              Right payload -> do writeChan log ("Why did they send a: " <> show payload)
                                  pure ConnClosed
              Left e -> writeChan log ("message - error " <> show e) >> pure ConnClosed


logger :: Chan String -> Bool -> IO ()
logger log True = forever $ readChan log >>= putStrLn . ((<>) "\n")
logger log False = forever $ readChan log >>= \_ -> pure ()

send :: Connection -> Payload -> Chan String -> IO ()
send conn payload log = do
  writeChan log ("message - sending " <> QL.unpack (encode payload))
  sendTextData conn (encode payload)

step :: Connection -> Chan String -> IO (Either ConnectionException Payload)
step conn log = try $ do
  msg' <- receiveData conn
  writeChan log ("message - received " <> QL.unpack msg')
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
    send conn (Heartbeat num) log
    threadDelay (interval * 1000)

setSequence :: IORef Integer -> Integer -> IO ()
setSequence key i = writeIORef key i

startEventStream :: Connection -> Chan Event -> Auth -> String -> Int -> Integer -> Chan String -> IO ConnLoopState
startEventStream conn events (Bot auth) seshID interval seqN log = do
  seqKey <- newIORef seqN
  heart <- forkIO $ heartbeat conn interval seqKey log

  let err :: SomeException -> IO ConnLoopState
      err e = writeChan log ("error - " <> show e) >> ConnReconnect auth seshID <$> readIORef seqKey
  handle err $ finally (eventStream (ConnData conn seshID auth events) seqKey log)
                       (killThread heart)

eventStream :: ConnectionData -> IORef Integer -> Chan String -> IO ConnLoopState
eventStream (ConnData conn seshID auth eventChan) seqKey log = loop Running
  where
  loop :: GatewayState -> IO ConnLoopState
  loop Running = do
    eitherPayload <- step conn log
    case eitherPayload :: Either ConnectionException Payload of
      Left (CloseRequest code str) ->
        case code of
          -- see discord documentation on gateway close event codes
          1000 -> ConnReconnect auth seshID <$> readIORef seqKey
          4000 -> ConnReconnect auth seshID <$> readIORef seqKey
          4006 -> pure ConnStart
          4007 -> ConnReconnect auth seshID <$> readIORef seqKey
          4014 -> ConnReconnect auth seshID <$> readIORef seqKey
          e -> do writeChan log ("Closing connection because $" <> show e <> " " <> show str)
                  pure ConnClosed
      Left _ -> ConnReconnect auth seshID <$> readIORef seqKey
      Right (Dispatch event sq) -> do
        setSequence seqKey sq
        writeChan eventChan event
        loop Running
      Right (Heartbeat sq) -> do
        setSequence seqKey sq
        send conn (Heartbeat sq) log
        loop Running
      Right (Reconnect)      -> writeChan log "Should reconnect" >> loop InvalidReconnect
      Right (InvalidSession) -> pure ConnStart
      Right (HeartbeatAck)   -> loop Running
      Right _ -> do writeChan log "Discord-hs.Gateway.Error - Invalid Packet"
                    loop InvalidDead
  loop InvalidReconnect = do writeChan log "should try and reconnect"
                             seqID <- readIORef seqKey
                             writeChan log ("Reconnecting to: " <> show (ConnReconnect auth seshID seqID))
                             pure (ConnReconnect auth seshID seqID)

  loop InvalidDead      = do writeChan log "Discord-hs.Gateway.Error - Bot died"
                             pure ConnClosed
