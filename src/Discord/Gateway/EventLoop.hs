{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_HADDOCK prune, not-home #-}

-- | Provides logic code for interacting with the Discord websocket
--   gateway. Realistically, this is probably lower level than most
--   people will need
module Discord.Gateway.EventLoop where

import Prelude hiding (log)

import Control.Monad (forever, (<=<))
import Control.Monad.Random (getRandomR)
import Control.Concurrent.Chan
import Control.Exception.Safe (try, finally, handle, SomeException)
import Control.Concurrent (threadDelay, killThread, forkIO)
import Data.Monoid ((<>))
import Data.IORef
import Data.Aeson (eitherDecode, encode)
import qualified Data.Text as T
import qualified Data.ByteString.Lazy.Char8 as QL

import Wuss (runSecureClient)
import Network.WebSockets (ConnectionException(..), Connection, receiveData, sendTextData)

import Discord.Types

data ConnLoopState = ConnStart
                   | ConnClosed
                   | ConnReconnect T.Text String Integer
  deriving Show

data ConnectionData = ConnData { connection :: Connection
                               , connSessionID :: String
                               , connAuth :: T.Text
                               , connChan :: Chan Event
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
            msg <- getPayload conn log
            case msg of
              Right (Hello interval) -> do
                send conn (Identify auth False 50 (0, 1)) log
                msg2 <- getPayload conn log
                case msg2 of
                  Right (Dispatch r@(Ready _ _ _ _ seshID) _) -> do
                    writeChan events r
                    startEventStream conn events auth seshID interval 0 log
                  _ -> writeChan log ("received2: " <> show msg2) >> pure ConnClosed
              _ -> writeChan log ("received1: " <> show msg) >> pure ConnClosed
      (ConnReconnect tok seshID seqID) -> do
          loop <=< runSecureClient "gateway.discord.gg" 443 "/?v=6&encoding=json" $ \conn -> do
            send conn (Resume tok seshID seqID) log
            writeChan log "Resuming???"
            eitherPayload <- getPayload conn log
            case eitherPayload of
              Right (Hello interval) -> startEventStream conn events auth seshID interval seqID log
              Right (InvalidSession retry) -> do t <- getRandomR (1,5)
                                                 writeChan log ("Invalid sesh, sleep:" <> show t)
                                                 threadDelay (t * 10^6)
                                                 pure $ if retry
                                                        then ConnReconnect tok seshID seqID
                                                        else ConnStart
              Right payload -> do writeChan log ("Why did they send a: " <> show payload)
                                  pure ConnClosed
              Left e -> writeChan log ("message - error " <> show e) >> pure ConnClosed


send :: Connection -> GatewaySendable -> Chan String -> IO ()
send conn payload log = do
  writeChan log ("message - sending " <> QL.unpack (encode payload))
  sendTextData conn (encode payload)

getPayload :: Connection -> Chan String -> IO (Either ConnectionException GatewayReceivable)
getPayload conn log = try $ do
  msg' <- receiveData conn
  writeChan log ("message - received " <> QL.unpack msg')
  case eitherDecode msg' of
    Right msg -> return msg
    Left  err -> do writeChan log ("parse error Error - " <> err)
                    writeChan log ("parse error Message - " <> show msg')
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
eventStream (ConnData conn seshID auth eventChan) seqKey log = loop
  where
  loop :: IO ConnLoopState
  loop = do
    eitherPayload <- getPayload conn log
    case eitherPayload :: Either ConnectionException GatewayReceivable of
      Left (CloseRequest code str) -> case code of
          -- see discord documentation on gateway close event codes
          1000 -> ConnReconnect auth seshID <$> readIORef seqKey
          4000 -> ConnReconnect auth seshID <$> readIORef seqKey
          4006 -> pure ConnStart
          4007 -> ConnReconnect auth seshID <$> readIORef seqKey
          4014 -> ConnReconnect auth seshID <$> readIORef seqKey
          e -> do writeChan log ("Closing connection because #" <> show e <> " " <> show str)
                  pure ConnClosed
      Left _ -> ConnReconnect auth seshID <$> readIORef seqKey
      Right (Dispatch event sq) -> do setSequence seqKey sq
                                      writeChan eventChan event
                                      loop
      Right (HeartbeatRequest sq) -> do setSequence seqKey sq
                                        send conn (Heartbeat sq) log
                                        loop
      Right (Reconnect)      -> do writeChan log "Should reconnect"
                                   ConnReconnect auth seshID <$> readIORef seqKey
      Right (InvalidSession retry) -> if retry
                                      then ConnReconnect auth seshID <$> readIORef seqKey
                                      else pure ConnStart
      Right (HeartbeatAck)   -> loop
      Right p -> writeChan log ("error - Invalid gateway payload: " <> show p) >> pure ConnClosed
