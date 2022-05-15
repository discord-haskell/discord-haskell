{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Main module of the library
-- Contains all the entrypoints
module Discord
  ( runDiscord
  , restCall
  , sendCommand
  , readCache
  , stopDiscord

  , DiscordHandler

  , DiscordHandle
  , Cache(..)
  , RestCallErrorCode(..)
  , RunDiscordOpts(..)
  , FromJSON
  , Request
  , def
  ) where

import Prelude hiding (log)
import Control.Monad.Reader (ReaderT, runReaderT, void, ask, liftIO, forever)
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

-- | A `ReaderT` wrapper around `DiscordHandle` and `IO`. Most functions act in
-- this monad
type DiscordHandler = ReaderT DiscordHandle IO

-- | Options for the connection. 
data RunDiscordOpts = RunDiscordOpts
  { -- | Token for the discord API
    discordToken :: T.Text
  , -- | Actions executed right after a connexion to discord's API is
    -- established
    discordOnStart :: DiscordHandler ()
  , -- | Actions executed at termination.
    --
    -- Note that this runs in plain `IO` and not in `DiscordHandler` as the
    -- connexion has been closed before this runs.
    --
    -- Useful for cleaning up.
    discordOnEnd :: IO ()
  , -- | Actions run upon the reception of an `Event`. This is here most of the
    -- code of the bot may get dispatched from.
    discordOnEvent :: Event -> DiscordHandler ()
  , -- | Dispatching on internal logs
    discordOnLog :: T.Text -> IO ()
  , -- | Fork a thread for every `Event` recived
    discordForkThreadForEvents :: Bool
  , -- | The gateway intents the bot is asking for
    discordGatewayIntent :: GatewayIntent
  }

-- | Default values for `RunDiscordOpts`
instance Default RunDiscordOpts where
  def = RunDiscordOpts { discordToken = ""
                       , discordOnStart = pure ()
                       , discordOnEnd = pure ()
                       , discordOnEvent = \_ -> pure ()
                       , discordOnLog = \_ -> pure ()
                       , discordForkThreadForEvents = True
                       , discordGatewayIntent = def
                       }

-- | Entrypoint to the library 
runDiscord :: RunDiscordOpts -> IO T.Text
runDiscord opts = do
  log <- newChan
  logId <- liftIO $ startLogger (discordOnLog opts) log
  (cache, cacheId) <- liftIO $ startCacheThread log
  (rest, restId) <- liftIO $ startRestThread (Auth (discordToken opts)) log
  (gate, gateId) <- liftIO $ startGatewayThread (Auth (discordToken opts)) (discordGatewayIntent opts) cache log

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

  finally (runDiscordLoop handle opts)
          (discordOnEnd opts >> runReaderT stopDiscord handle)

-- | Runs the main loop 
runDiscordLoop :: DiscordHandle -> RunDiscordOpts -> IO T.Text
runDiscordLoop handle opts = do
  resp <- liftIO $ writeRestCall (discordHandleRestChan handle) GetCurrentUser
  case resp of
    Left (RestCallInternalErrorCode c e1 e2) -> libError $
             "HTTP Error Code " <> T.pack (show c) <> " " <> TE.decodeUtf8 e1
                                                   <> " " <> TE.decodeUtf8 e2
    Left (RestCallInternalHttpException e) -> libError ("HTTP Exception -  " <> T.pack (show e))
    Left (RestCallInternalNoParse _ _) -> libError "Couldn't parse GetCurrentUser"
    _ -> do me <- liftIO . runReaderT (try $ discordOnStart opts) $ handle
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
                 action $ do me <- liftIO . runReaderT (try $ discordOnEvent opts userEvent) $ handle
                             case me of
                               Left (e :: SomeException) -> writeChan (discordHandleLog handle)
                                         ("eventhandler - crashed on [" <> T.pack (show userEvent) <> "] "
                                          <> "          with error: "  <> T.pack (show e))
                               Right _ -> pure ()
                 loop

-- | A Error code following a rest call
data RestCallErrorCode = RestCallErrorCode Int T.Text T.Text
  deriving (Show, Read, Eq, Ord)

-- | Execute one http request and get a response
restCall :: (Request (r a), FromJSON a) => r a -> DiscordHandler (Either RestCallErrorCode a)
restCall r = do h <- ask
                empty <- isEmptyMVar (discordHandleLibraryError h)
                if not empty
                then pure (Left (RestCallErrorCode 400 "Library Stopped Working" ""))
                else do
                    resp <- liftIO $ writeRestCall (discordHandleRestChan h) r
                    case resp of
                      Right x -> pure (Right x)
                      Left (RestCallInternalErrorCode c e1 e2) -> do
                        pure (Left (RestCallErrorCode c (TE.decodeUtf8 e1) (TE.decodeUtf8 e2)))
                      Left (RestCallInternalHttpException _) ->
                        threadDelay (10 * 10^(6 :: Int)) >> restCall r
                      Left (RestCallInternalNoParse err dat) -> do
                        let formaterr = T.pack ("restcall - parse exception [" <> err <> "]"
                                              <> " while handling" <> show dat)
                        writeChan (discordHandleLog h) formaterr
                        pure (Left (RestCallErrorCode 400 "Library Parse Exception" formaterr))

-- | Send a user GatewaySendable
sendCommand :: GatewaySendable -> DiscordHandler ()
sendCommand e = do
  h <- ask
  writeChan (gatewayHandleUserSendables (discordHandleGateway h)) e
  case e of
    UpdateStatus opts -> liftIO $ writeIORef (gatewayHandleLastStatus (discordHandleGateway h)) (Just opts)
    _ -> pure ()

-- | Access the current state of the gateway cache
readCache :: DiscordHandler Cache
readCache = do
  h <- ask
  merr <- readMVar (cacheHandleCache (discordHandleCache h))
  case merr of
    Left (c, _) -> pure c
    Right c -> pure c


-- | Stop all the background threads
stopDiscord :: DiscordHandler ()
stopDiscord = do h <- ask
                 _ <- tryPutMVar (discordHandleLibraryError h) "Library has closed"
                 threadDelay (10^(6 :: Int) `div` 10)
                 mapM_ (killThread . toId) (discordHandleThreads h)
  where toId t = case t of
                   HandleThreadIdRest a -> a
                   HandleThreadIdGateway a -> a
                   HandleThreadIdCache a -> a
                   HandleThreadIdLogger a -> a

-- | Starts the internal logger
startLogger :: (T.Text -> IO ()) -> Chan T.Text -> IO ThreadId
startLogger handle logC = forkIO $ forever $
  do me <- try $ readChan logC >>= handle
     case me of
       Right _ -> pure ()
       Left (_ :: IOException) ->
         -- writeChan logC "Log handler failed"
         pure ()

