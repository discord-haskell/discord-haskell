{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Discord
  ( runDiscord
  , restCall
  , sendCommand
  , readCache
  , stopDiscord

  , DiscordHandle
  , Cache(..)
  , RestCallErrorCode(..)
  , RunDiscordOpts(..)
  , FromJSON
  , def
  ) where

import Prelude hiding (log)

import Control.Monad (void, forever)
import Data.Aeson (FromJSON)
import Data.Default (Default, def)
import Data.IORef (writeIORef)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

import UnliftIO (race, try, finally, SomeException, IOException)
import UnliftIO.Concurrent

import Discord.Handle
import Discord.Internal.Rest
import Discord.Internal.Rest.User (UserRequest(GetCurrentUser))
import Discord.Internal.Gateway

data RunDiscordOpts = RunDiscordOpts
  { discordToken :: T.Text
  , discordOnStart :: DiscordHandle -> IO ()
  , discordOnEnd :: IO ()
  , discordOnEvent :: DiscordHandle -> Event -> IO ()
  , discordOnLog :: T.Text -> IO ()
  , discordForkThreadForEvents :: Bool
  , discordGatewayIntent :: GatewayIntent
  }

instance Default RunDiscordOpts where
  def = RunDiscordOpts { discordToken = ""
                       , discordOnStart = \_ -> pure ()
                       , discordOnEnd = pure ()
                       , discordOnEvent = \_ _ -> pure ()
                       , discordOnLog = \_ -> pure ()
                       , discordForkThreadForEvents = True
                       , discordGatewayIntent = def
                       }

runDiscord :: RunDiscordOpts -> IO T.Text
runDiscord opts = do
  log <- newChan
  logId <- startLogger (discordOnLog opts) log
  (cache, cacheId) <- startCacheThread log
  (rest, restId) <- startRestThread (Auth (discordToken opts)) log
  (gate, gateId) <- startGatewayThread (Auth (discordToken opts)) (discordGatewayIntent opts) cache log

  libE <- newEmptyMVar

  let handle = DiscordHandle { discordHandleRestChan = rest
                             , discordHandleGateway = gate
                             , discordHandleCache = cache
                             , discordHandleLog = log
                             , discordHandleLibraryError = libE
                             , discordHandleThreads =
                                 [ HandleThreadIdLogger logId
                                 , HandleThreadIdRest restId
                                 , HandleThreadIdCache cacheId
                                 , HandleThreadIdGateway gateId
                                 ]
                             }

  finally (runDiscordLoop opts handle)
          (discordOnEnd opts >> stopDiscord handle)

runDiscordLoop :: RunDiscordOpts -> DiscordHandle -> IO T.Text
runDiscordLoop opts handle = do
  resp <- writeRestCall (discordHandleRestChan handle) GetCurrentUser
  case resp of
    Left (RestCallInternalErrorCode c e1 e2) -> libError $
             "HTTP Error Code " <> T.pack (show c) <> " " <> TE.decodeUtf8 e1
                                                   <> " " <> TE.decodeUtf8 e2
    Left (RestCallInternalHttpException e) -> libError ("HTTP Exception -  " <> T.pack (show e))
    Left (RestCallInternalNoParse _ _) -> libError "Couldn't parse GetCurrentUser"
    _ -> do me <- try $ discordOnStart opts handle
            case me of
              Left (e :: SomeException) -> libError ("discordOnStart handler stopped on an exception:\n\n" <> T.pack (show e))
              Right _ -> loop
 where
   libError :: T.Text -> IO T.Text
   libError msg = tryPutMVar (discordHandleLibraryError handle) msg >> pure msg

   loop :: IO T.Text
   loop = do next <- race (readMVar (discordHandleLibraryError handle))
                          (readChan (gatewayHandleEvents (discordHandleGateway handle)))
             case next of
               Left err -> libError err
               Right (Left err) -> libError (T.pack (show err))
               Right (Right event) -> do
                 let userEvent = userFacingEvent event
                 let action = if discordForkThreadForEvents opts then void . forkIO
                                                                 else id
                 action $ do me <- try $ discordOnEvent opts handle userEvent
                             case me of
                               Left (e :: SomeException) -> writeChan (discordHandleLog handle)
                                         ("eventhandler - crashed on [" <> T.pack (show userEvent) <> "] "
                                          <> "          with error: "  <> T.pack (show e))
                               Right _ -> pure ()
                 loop


data RestCallErrorCode = RestCallErrorCode Int T.Text T.Text
  deriving (Show, Read, Eq, Ord)

-- | Execute one http request and get a response
restCall :: (FromJSON a, Request (r a)) => DiscordHandle -> r a -> IO (Either RestCallErrorCode a)
restCall h r = do empty <- isEmptyMVar (discordHandleLibraryError h)
                  if not empty
                  then pure (Left (RestCallErrorCode 400 "Library Stopped Working" ""))
                  else do
                    resp <- writeRestCall (discordHandleRestChan h) r
                    case resp of
                      Right x -> pure (Right x)
                      Left (RestCallInternalErrorCode c e1 e2) -> do
                        pure (Left (RestCallErrorCode c (TE.decodeUtf8 e1) (TE.decodeUtf8 e2)))
                      Left (RestCallInternalHttpException _) ->
                        threadDelay (10 * 10^(6 :: Int)) >> restCall h r
                      Left (RestCallInternalNoParse err dat) -> do
                        let formaterr = T.pack ("restcall - parse exception [" <> err <> "]"
                                              <> " while handling" <> show dat)
                        writeChan (discordHandleLog h) formaterr
                        pure (Left (RestCallErrorCode 400 "Library Parse Exception" formaterr))

-- | Send a user GatewaySendable
sendCommand :: DiscordHandle -> GatewaySendable -> IO ()
sendCommand h e = do
  writeChan (gatewayHandleUserSendables (discordHandleGateway h)) e
  case e of
    UpdateStatus opts -> writeIORef (gatewayHandleLastStatus (discordHandleGateway h)) (Just opts)
    _ -> pure ()

-- | Access the current state of the gateway cache
readCache :: DiscordHandle -> IO Cache
readCache h = do
  merr <- readMVar (cacheHandleCache (discordHandleCache h))
  case merr of
    Left (c, _) -> pure c
    Right c -> pure c


-- | Stop all the background threads
stopDiscord :: DiscordHandle -> IO ()
stopDiscord h = do _ <- tryPutMVar (discordHandleLibraryError h) "Library has closed"
                   threadDelay (10^(6 :: Int) `div` 10)
                   mapM_ (killThread . toId) (discordHandleThreads h)
  where toId t = case t of
                   HandleThreadIdRest a -> a
                   HandleThreadIdGateway a -> a
                   HandleThreadIdCache a -> a
                   HandleThreadIdLogger a -> a

startLogger :: (T.Text -> IO ()) -> Chan T.Text -> IO ThreadId
startLogger handle logC = forkIO $ forever $
  do me <- try $ readChan logC >>= handle
     case me of
       Right _ -> pure ()
       Left (_ :: IOException) ->
         -- writeChan logC "Log handler failed"
         pure ()

