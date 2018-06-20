{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_HADDOCK prune, not-home #-}

-- | Provides logic code for interacting with the Discord websocket
--   gateway. Realistically, this is probably lower level than most
--   people will need
module Network.Discord.Gateway.Internal where

import Prelude hiding (log)

import Control.Monad (forever, (<=<))
import Control.Monad.Random (getRandomR)
import Control.Concurrent.Chan
import Control.Exception.Safe (try, finally, SomeException)
import Control.Concurrent (threadDelay, killThread, forkIO)
import Data.Monoid ((<>))
import Data.IORef
import Data.Aeson (eitherDecode, encode)
import qualified Data.ByteString.Char8 as Q

import Wuss (runSecureClient)
import Network.WebSockets (Connection, receiveData, sendTextData)

import Network.Discord.Types

data GatewayState = Running
                  | InvalidReconnect
                  | InvalidDead

data ConnLoopState = ConnStart
                   | ConnClosed
                   | ConnReconnect Q.ByteString String Integer
  deriving Show

data ConnectionData = ConnData { connection :: Connection
                               , connSessionID :: String
                               , connAuth :: Auth
                               , connChan :: (Chan Event)
                               }

connectionLoop :: Auth -> Chan Event -> Chan String -> ConnLoopState -> IO ()
connectionLoop auth events log = loop
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
                case parseDispatch <$> msg2 of
                  Right (Right payload) ->
                    case payload of
                      (Ready (Init _ _ _ _ seshID)) ->
                        startEventStream conn events auth seshID interval (-1) log
                      _ -> writeChan log ("received: " <> show msg) >> pure ConnClosed
                  err -> writeChan log ("Ready event parse error " <> show err <> " on " <> show msg2) >> pure ConnClosed
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
logger log True = forever $ readChan log >>= putStrLn
logger _ False = pure ()

step :: Connection -> Chan String -> IO (Either SomeException Payload)
step conn log = try $ do
  msg' <- receiveData conn
  writeChan log ("message - received " <> show msg')
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
    send conn (Heartbeat num) log
    writeChan log ("beat " <> show interval)
    threadDelay (interval * 1000)

setSequence :: IORef Integer -> Integer -> IO ()
setSequence key i = writeIORef key i

send :: Connection -> Payload -> Chan String -> IO ()
send conn payload log = do
  writeChan log ("message - sending " <> show payload)
  sendTextData conn (encode payload)

startEventStream :: Connection -> Chan Event -> Auth -> String -> Int -> Integer -> Chan String -> IO ConnLoopState
startEventStream conn events auth seshID interval seqN log = do
    seqKey <- newIORef seqN
    heart <- forkIO $ heartbeat conn interval seqKey log
    finally (eventStream (ConnData conn seshID auth events) seqKey log)
            (killThread heart)

eventStream :: ConnectionData -> IORef Integer -> Chan String -> IO ConnLoopState
eventStream (ConnData conn seshID auth eventChan) seqKey log = loop Running
  where
  loop :: GatewayState -> IO ConnLoopState
  loop Running = do
    eitherPayload <- step conn log
    case eitherPayload :: Either SomeException Payload of
      Left e -> do writeChan log ("Unknown Exception - " <> show e)
                   threadDelay (round (1/2 * 10^6))
                   loop InvalidReconnect
      Right (Dispatch obj sq name) -> do
        setSequence seqKey sq
        case parseDispatch (Dispatch obj sq name) of
          Left reason -> writeChan log ("Discord-hs.Gateway.Dispatch - " <> reason)
          Right event -> writeChan eventChan event
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
                             let (Bot tok) = auth
                             seqID <- readIORef seqKey
                             writeChan log ("Reconnecting to: " <> show (ConnReconnect tok seshID seqID))
                             pure (ConnReconnect tok seshID seqID)

  loop InvalidDead      = do writeChan log "Discord-hs.Gateway.Error - Bot died"
                             pure ConnClosed
