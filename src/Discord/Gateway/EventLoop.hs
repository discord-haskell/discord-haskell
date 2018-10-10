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
                           receiveData, sendTextData)

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
-- | Securely run a connection IO action. Send a close on exception
connect :: (Connection -> IO a) -> IO a
connect = runSecureClient "gateway.discord.gg" 443 "/?v=6&encoding=json"

connectionLoop :: Auth -> Chan Event -> Chan GatewaySendable -> Chan String -> IO ()
connectionLoop auth events userSend log = loop ConnStart
 where
  loop :: ConnLoopState -> IO ()
  loop s = do
    writeChan log ("gateway - connection loop state " <> show s)
    case s of
      (ConnClosed) -> pure ()
      (ConnStart) -> do
          loop <=< connect $ \conn -> do
            msg <- getPayload conn log
            case msg of
              Right (Hello interval) -> do
                sendTextData conn (encode (Identify auth False 50 (0, 1)))
                msg2 <- getPayload conn log
                case msg2 of
                  Right (Dispatch r@(Ready _ _ _ _ seshID) _) -> do
                    writeChan events r
                    startEventStream conn events auth seshID interval 0 userSend log
                  _ -> writeChan log ("gateway - connstart must be ready: " <> show msg2) >> pure ConnClosed
              _ -> writeChan log ("gateway - connstart must be hello: " <> show msg) >> pure ConnClosed

      (ConnReconnect tok seshID seqID) -> do
          next <- try $ connect $ \conn -> do
              sendTextData conn (encode (Resume tok seshID seqID))
              eitherPayload <- getPayload conn log
              case eitherPayload of
                  Right (Hello interval) ->
                      startEventStream conn events auth seshID interval seqID userSend log
                  Right (InvalidSession retry) -> do
                      t <- getRandomR (1,5)
                      threadDelay (t * 10^6)
                      pure $ if retry
                             then ConnReconnect tok seshID seqID
                             else ConnStart
                  Right payload -> do
                      writeChan log ("gateway - connreconnect invalid response: " <> show payload)
                      pure ConnClosed
                  Left e ->
                      writeChan log ("gateway - connreconnect error " <> show e) >> pure ConnClosed
          case next :: Either SomeException ConnLoopState of
            Left e -> do writeChan log ("gateway - connreconnect after eventStream error: " <> show e)
                         t <- getRandomR (3,10)
                         threadDelay (t * 10^6)
                         loop (ConnReconnect tok seshID seqID)
            Right n -> loop n


getPayloadTimeout :: Connection -> Int -> Chan String -> IO (Either ConnectionException GatewayReceivable)
getPayloadTimeout conn interval log = do
  res <- race (threadDelay ((interval * 1000 * 3) `div` 2))
              (getPayload conn log)
  case res of
    Left () -> pure (Right Reconnect)
    Right other -> pure other

getPayload :: Connection -> Chan String -> IO (Either ConnectionException GatewayReceivable)
getPayload conn log = try $ do
  msg' <- receiveData conn
  writeChan log ("gateway - received " <> QL.unpack msg')
  case eitherDecode msg' of
    Right msg -> return msg
    Left  err -> do writeChan log ("gateway - received parse Error - " <> err)
                    return (ParseError err)

heartbeat :: Chan GatewaySendable -> Int -> IORef Integer -> Chan String -> IO ()
heartbeat send interval seqKey log = do
  threadDelay (1 * 10^6)
  writeChan log "gateway - starting heartbeat"
  forever $ do
    num <- readIORef seqKey
    writeChan send (Heartbeat num)
    threadDelay (interval * 1000)

setSequence :: IORef Integer -> Integer -> IO ()
setSequence key i = writeIORef key i

startEventStream :: Connection -> Chan Event -> Auth -> String -> Int
                               -> Integer -> Chan GatewaySendable -> Chan String -> IO ConnLoopState
startEventStream conn events (Auth auth) seshID interval seqN userSend log = do
  seqKey <- newIORef seqN
  let err :: SomeException -> IO ConnLoopState
      err e = do writeChan log ("gateway - eventStream error: " <> show e)
                 ConnReconnect auth seshID <$> readIORef seqKey
  handle err $ do
    gateSends <- newChan
    sendsId <- forkIO $ sendableLoop conn (Sendables userSend gateSends) log
    heart <- forkIO $ heartbeat gateSends interval seqKey log

    finally (eventStream (ConnData conn seshID auth events) seqKey interval gateSends log)
            (killThread heart >> killThread sendsId)

eventStream :: ConnectionData -> IORef Integer -> Int -> Chan GatewaySendable
                              -> Chan String -> IO ConnLoopState
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
          e -> do writeChan log ("gateway - Closing connection because #"
                                         <> show e <> " " <> show str)
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
      Right p -> do writeChan log ("gateway - Invalid gateway payload: " <> show p)
                    pure ConnClosed

data Sendables = Sendables { -- | Things the user wants to send. Doesn't reset on reconnect
                             userSends :: Chan GatewaySendable -- ^ Things the user wants to send
                            -- | Things the library needs to send. Resets to empty on reconnect
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
