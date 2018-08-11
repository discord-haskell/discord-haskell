{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_HADDOCK prune, not-home #-}

-- | Provides logic code for interacting with the Discord websocket
--   gateway. Realistically, this is probably lower level than most
--   people will need
module Discord.Gateway.EventLoop where

import Prelude hiding (log)

import Control.Monad (forever, (<=<))
import Control.Monad.Random (getRandomR)
import Control.Concurrent.Async (race)
import Control.Concurrent.Chan
import Control.Exception.Safe (try, finally, handle, SomeException)
import Control.Concurrent (threadDelay, killThread, forkIO)
import Data.Monoid ((<>))
import Data.IORef
import Data.Aeson (eitherDecode, encode)
import qualified Data.Text as T
import qualified Data.ByteString.Lazy.Char8 as QL

import Wuss (runSecureClient)
import Network.WebSockets (ConnectionException(..), Connection,
                           sendClose, receiveData, sendTextData)

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

data Sendables = Sendables { userSends :: Chan GatewaySendable
                           , gatewaySends :: Chan GatewaySendable
                           }

sendableLoop :: Connection -> Sendables -> Chan [Char] -> IO ()
sendableLoop conn sends log = forever $ do
  -- send a ~120 events a min by delaying
  threadDelay (round (10^6 * (62 / 120)))
  let e :: Either GatewaySendable GatewaySendable -> GatewaySendable
      e = either id id
  payload <- e <$> race (readChan (userSends sends)) (readChan (gatewaySends sends))
  writeChan log ("gateway - sending " <> QL.unpack (encode payload))
  sendTextData conn (encode payload)

-- | Securely run a connection IO action. Send a close on exception
connect :: Chan GatewaySendable -> Chan String
                -> (Connection -> Chan GatewaySendable -> IO a) -> IO a
connect sends log app = runSecureClient "gateway.discord.gg" 443 "/?v=6&encoding=json" $ \conn -> do
  gateSends <- newChan
  sendsId <- forkIO (sendableLoop conn (Sendables sends gateSends) log)
  finally (app conn gateSends)
          (sendClose conn ("" :: T.Text) >> killThread sendsId)

connectionLoop :: Auth -> Chan Event -> Chan GatewaySendable -> Chan String -> IO ()
connectionLoop auth events userSend log = loop ConnStart
 where
  loop :: ConnLoopState -> IO ()
  loop s = do
    writeChan log ("conn loop: " <> show s)
    case s of
      (ConnClosed) -> writeChan log "Conn Closed"
      (ConnStart) -> do
          loop <=< connect userSend log $ \conn send -> do
            msg <- getPayload conn log
            case msg of
              Right (Hello interval) -> do
                sendTextData conn (encode (Identify auth False 50 (0, 1)))
                msg2 <- getPayload conn log
                case msg2 of
                  Right (Dispatch r@(Ready _ _ _ _ seshID) _) -> do
                    writeChan events r
                    startEventStream conn events auth seshID interval 0 send log
                  _ -> writeChan log ("received2: " <> show msg2) >> pure ConnClosed
              _ -> writeChan log ("received1: " <> show msg) >> pure ConnClosed

      (ConnReconnect tok seshID seqID) -> do
          next <- try $ connect userSend log $ \conn send -> do
              writeChan log "Resuming???"
              sendTextData conn (encode (Resume tok seshID seqID))
              eitherPayload <- getPayload conn log
              case eitherPayload of
                  Right (Hello interval) ->
                      startEventStream conn events auth seshID interval seqID send log
                  Right (InvalidSession retry) -> do
                      t <- getRandomR (1,5)
                      writeChan log ("Invalid sesh, sleep:" <> show t)
                      threadDelay (t * 10^6)
                      pure $ if retry
                             then ConnReconnect tok seshID seqID
                             else ConnStart
                  Right payload -> do
                      writeChan log ("Why did they send a: " <> show payload)
                      pure ConnClosed
                  Left e ->
                      writeChan log ("message - error " <> show e) >> pure ConnClosed
          case next :: Either SomeException ConnLoopState of
            Left e -> do writeChan log ("exception - connecting: " <> show e)
                         t <- getRandomR (3,10)
                         threadDelay (t * 10^6)
                         loop (ConnReconnect tok seshID seqID)
            Right n -> loop n


getPayloadTimeout :: Connection -> Int -> Chan String -> IO (Either ConnectionException GatewayReceivable)
getPayloadTimeout conn interval log = do
  res <- race (threadDelay (interval * 1000 * 3 `div` 2))
              (getPayload conn log)
  case res of
    Left () -> pure (Right (InvalidSession True))
    Right other -> pure other

getPayload :: Connection -> Chan String -> IO (Either ConnectionException GatewayReceivable)
getPayload conn log = try $ do
  msg' <- receiveData conn
  writeChan log ("message - received " <> QL.unpack msg')
  case eitherDecode msg' of
    Right msg -> return msg
    Left  err -> do writeChan log ("parse error Error - " <> err)
                    writeChan log ("parse error Message - " <> show msg')
                    return (ParseError err)

heartbeat :: Chan GatewaySendable -> Int -> IORef Integer -> Chan String -> IO ()
heartbeat send interval seqKey log = do
  writeChan log "starting the heartbeat"
  forever $ do
    num <- readIORef seqKey
    writeChan send (Heartbeat num)
    threadDelay (interval * 1000)

setSequence :: IORef Integer -> Integer -> IO ()
setSequence key i = writeIORef key i

startEventStream :: Connection -> Chan Event -> Auth -> String -> Int
                               -> Integer -> Chan GatewaySendable -> Chan String -> IO ConnLoopState
startEventStream conn events (Auth auth) seshID interval seqN send log = do
  seqKey <- newIORef seqN
  heart <- forkIO $ heartbeat send interval seqKey log

  let err :: SomeException -> IO ConnLoopState
      err e = do writeChan log ("error - " <> show e)
                 ConnReconnect auth seshID <$> readIORef seqKey
  handle err $ finally (eventStream (ConnData conn seshID auth events) seqKey interval send log)
                       (killThread heart)

eventStream :: ConnectionData -> IORef Integer -> Int -> Chan GatewaySendable -> Chan String -> IO ConnLoopState
eventStream (ConnData conn seshID auth eventChan) seqKey interval send log = loop
  where
  loop :: IO ConnLoopState
  loop = do
    eitherPayload <- getPayloadTimeout conn interval log
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
                                        writeChan send (Heartbeat sq)
                                        loop
      Right (Reconnect)      -> do writeChan log "Should reconnect"
                                   ConnReconnect auth seshID <$> readIORef seqKey
      Right (InvalidSession retry) -> if retry
                                      then ConnReconnect auth seshID <$> readIORef seqKey
                                      else pure ConnStart
      Right (HeartbeatAck)   -> loop
      Right p -> writeChan log ("error - Invalid gateway payload: " <> show p) >> pure ConnClosed
