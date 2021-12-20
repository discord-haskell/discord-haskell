{-# LANGUAGE OverloadedStrings #-}

-- | Provides logic code for interacting with the Discord websocket
--   gateway. Realistically, this is probably lower level than most
--   people will need
module Discord.Internal.Gateway.EventLoop where

import Prelude hiding (log)

import Control.Monad (forever, void)
import Control.Monad.Random (getRandomR)
import Control.Concurrent.Async (race)
import Control.Concurrent.Chan
import Control.Concurrent (threadDelay, killThread, forkIO)
import Control.Exception.Safe (try, finally, SomeException)
import Data.IORef
import Data.Aeson (eitherDecode, encode)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Lazy as BL

import Wuss (runSecureClient)
import Network.WebSockets (ConnectionException(..), Connection,
                           receiveData, sendTextData, sendClose)

import Discord.Internal.Types


data GatewayHandle = GatewayHandle
  { gatewayHandleEvents         :: Chan (Either GatewayException EventInternalParse)
  , gatewayHandleUserSendables  :: Chan GatewaySendable
  , gatewayHandleLastStatus     :: IORef (Maybe UpdateStatusOpts)
  , gatewayHandleLastSequenceId :: IORef Integer
  , gatewayHandleSessionId      :: IORef T.Text
  }

data GatewayException = GatewayExceptionCouldNotConnect T.Text
                      | GatewayExceptionEventParseError T.Text T.Text
                      | GatewayExceptionUnexpected GatewayReceivable T.Text
                      | GatewayExceptionConnection ConnectionException T.Text
  deriving (Show)


-- | State of the eventloop
data LoopState = LoopStart
               | LoopClosed
               | LoopReconnect
  deriving Show

-- | Enough info for library to send info to discord.
data SendablesData = SendablesData
  { sendableConnection :: Connection
  , librarySendables :: Chan GatewaySendableInternal
  , startsendingUsers :: IORef Bool
  , heartbeatInterval :: Integer
  }

{-
Some quick documentation for some of the variables passed around:

Auth                                                         needed to connect
GatewayIntent                                                needed to connect
GatewayHandle (eventsGifts,status,usersends,seq,sesh)        needed all over
log :: Chan (T.Text)                                         needed all over

sendableConnection                                 set by setup,  need sendableLoop
librarySendables :: Chan (GatewaySendableInternal) set by setup,  need heartbeat
heartbeatInterval :: Int                           set by Hello,  need heartbeat

sequenceId :: Int id of last event received        set by Resume, need heartbeat and reconnect
sessionId :: Text                                  set by Ready,  need reconnect
-}

connectionLoop :: Auth -> GatewayIntent -> GatewayHandle -> Chan T.Text -> IO ()
connectionLoop auth intent gatewayHandle log = outerloop LoopStart
  where

  outerloop :: LoopState -> IO ()
  outerloop state = do
      mfirst <- firstmessage state
      case mfirst of
        Nothing -> pure ()
        Just first -> do
            next <- try (startconnectionpls first)
            case next :: Either SomeException LoopState of
              Left _ -> do t <- getRandomR (3,20)
                           threadDelay (t * (10^(6 :: Int)))
                           writeChan log ("gateway - trying to reconnect after failure(s)")
                           outerloop LoopReconnect
              Right n -> outerloop n

  firstmessage :: LoopState -> IO (Maybe GatewaySendableInternal)
  firstmessage state =
    case state of
      LoopStart -> pure $ Just $ Identify auth intent (0, 1)
      LoopReconnect -> do seqId  <- readIORef (gatewayHandleLastSequenceId gatewayHandle)
                          seshId <- readIORef (gatewayHandleSessionId gatewayHandle)
                          if seshId == ""
                          then do writeChan log ("gateway - WARNING seshID was not set by READY?")
                                  pure $ Just $ Identify auth intent (0, 1)
                          else pure $ Just $ Resume auth seshId seqId
      LoopClosed -> pure Nothing

  startconnectionpls :: GatewaySendableInternal -> IO LoopState
  startconnectionpls first = runSecureClient "gateway.discord.gg" 443 "/?v=8&encoding=json" $ \conn -> do
                      msg <- getPayload conn log
                      case msg of
                        Right (Hello interval) -> do

                          internal <- newChan :: IO (Chan GatewaySendableInternal)
                          us <- newIORef False
                          -- start event loop
                          let sending = SendablesData conn internal us interval
                          sendsId <- forkIO $ sendableLoop conn gatewayHandle sending log
                          heart <- forkIO $ heartbeat sending (gatewayHandleLastSequenceId gatewayHandle)

                          writeChan internal first
                          finally (runEventLoop gatewayHandle sending log)
                                  (killThread heart >> killThread sendsId)
                        _ -> do
                          writeChan log "gateway - WARNING could not connect. Expected hello"
                          sendClose conn ("expected hello" :: BL.ByteString)
                          void $ forever $ void (receiveData conn :: IO BL.ByteString)
                          -- > after sendClose you should call receiveDataMessage until
                          -- > it throws an exception
                          -- haskell websockets documentation
                          threadDelay (3 * (10^(6 :: Int)))
                          pure LoopStart


runEventLoop :: GatewayHandle -> SendablesData -> Chan T.Text -> IO LoopState
runEventLoop thehandle sendablesData log = do loop
  where
  eventChan = gatewayHandleEvents thehandle

  loop = do
    eitherPayload <- getPayloadTimeout sendablesData log
    case eitherPayload :: Either ConnectionException GatewayReceivable of
      Right (Hello _interval) -> do writeChan log ("eventloop - unexpected hello")
                                    loop
      Right (Dispatch event sq) -> do writeIORef (gatewayHandleLastSequenceId thehandle) sq
                                      writeChan eventChan (Right event)
                                      case event of
                                        (InternalReady _ _ _ _ seshID _ _) ->
                                            writeIORef (gatewayHandleSessionId thehandle) seshID
                                        _ -> writeIORef (startsendingUsers sendablesData) True
                                      loop
      Right (HeartbeatRequest sq) -> do writeIORef (gatewayHandleLastSequenceId thehandle) sq
                                        writeChan (librarySendables sendablesData) (Heartbeat sq)
                                        loop
      Right (Reconnect)      -> pure LoopReconnect
      Right (InvalidSession retry) -> pure $ if retry then LoopReconnect else LoopStart
      Right (HeartbeatAck)   -> loop
      Right (ParseError e) -> do writeChan eventChan (Left (GatewayExceptionEventParseError e
                                                             "Normal event loop"))
                                 pure LoopClosed
      Left (CloseRequest code str) -> case code of
          -- see Discord and MDN documentation on gateway close event codes
          -- https://discord.com/developers/docs/topics/opcodes-and-status-codes#gateway-gateway-close-event-codes
          -- https://developer.mozilla.org/en-US/docs/Web/API/CloseEvent#properties
          1000 -> pure LoopReconnect
          1001 -> pure LoopReconnect
          4000 -> pure LoopReconnect
          4006 -> pure LoopStart
          4007 -> pure LoopStart
          4014 -> do writeChan eventChan (Left (GatewayExceptionUnexpected (Hello 0) $
                           "Tried to declare an unauthorized GatewayIntent. " <>
                           "Use the discord app manager to authorize by following: " <>
                           "https://github.com/aquarial/discord-haskell/issues/76"))
                     pure LoopClosed
          _ -> do writeChan eventChan (Left (GatewayExceptionConnection (CloseRequest code str)
                                              "Unknown close code. Closing connection. Consider opening an issue with discord-haskell"))
                  pure LoopClosed
      Left _ -> pure LoopReconnect


heartbeat :: SendablesData -> IORef Integer -> IO ()
heartbeat sendablesData seqKey = do
  threadDelay (3 * 10^(6 :: Int))
  forever $ do
    num <- readIORef seqKey
    writeChan (librarySendables sendablesData) (Heartbeat num)
    threadDelay (fromInteger (heartbeatInterval sendablesData * 1000))

getPayloadTimeout :: SendablesData -> Chan T.Text -> IO (Either ConnectionException GatewayReceivable)
getPayloadTimeout sendablesData log = do
  let interval = heartbeatInterval sendablesData
  res <- race (threadDelay (fromInteger ((interval * 1000 * 3) `div` 2)))
              (getPayload (sendableConnection sendablesData) log)
  case res of
    Left () -> pure (Right Reconnect)
    Right other -> pure other

getPayload :: Connection -> Chan T.Text -> IO (Either ConnectionException GatewayReceivable)
getPayload conn log = try $ do
  msg' <- receiveData conn
  case eitherDecode msg' of
    Right msg -> pure msg
    Left  err -> do writeChan log ("gateway - received parse Error - " <> T.pack err
                                      <> " while decoding " <> TE.decodeUtf8 (BL.toStrict msg'))
                    pure (ParseError (T.pack err))


-- simple idea: send payloads from user/sys to connection
-- has to be complicated though
sendableLoop :: Connection -> GatewayHandle -> SendablesData -> Chan T.Text -> IO ()
sendableLoop conn ghandle sendablesData _log = sendSysLoop
  where
  sendSysLoop = do
      threadDelay $ round ((10^(6 :: Int)) * (62 / 120) :: Double)
      payload <- readChan (librarySendables sendablesData)
      sendTextData conn (encode payload)
   -- writeChan _log ("gateway - sending " <> TE.decodeUtf8 (BL.toStrict (encode payload)))
      usersending <- readIORef (startsendingUsers sendablesData)
      if not usersending
      then sendSysLoop
      else do act <- readIORef (gatewayHandleLastStatus ghandle)
              case act of Nothing -> pure ()
                          Just opts -> sendTextData conn (encode (UpdateStatus opts))
              sendUserLoop

  sendUserLoop = do
   -- send a ~120 events a min by delaying
      threadDelay $ round ((10^(6 :: Int)) * (62 / 120) :: Double)
   -- payload :: Either GatewaySendableInternal GatewaySendable
      payload <- race (readChan (gatewayHandleUserSendables ghandle)) (readChan (librarySendables sendablesData))
      sendTextData conn (either encode encode payload)
   -- writeChan _log ("gateway - sending " <> TE.decodeUtf8 (BL.toStrict (either encode encode payload)))
      sendUserLoop
