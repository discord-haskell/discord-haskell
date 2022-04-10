{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Discord.Monad
  ( MonadDiscord (..),
    EnvRunDiscordOpts (..),
    RunDiscordOpts,
    runDiscord,
    runDiscordM,
    runDiscordMPure,
  )
where

import Control.Monad.Catch (MonadMask, finally, try)
import Control.Monad.Identity (IdentityT)
import Control.Monad.Reader
import Control.Monad.State (StateT)
import Data.Aeson (FromJSON)
import Data.Default (Default (..))
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Discord as D
import Discord.Handle
import Discord.Internal.Gateway
import Discord.Internal.Rest
import Discord.Requests
import UnliftIO (IOException, MonadUnliftIO, SomeException, race, toIO)
import UnliftIO.Concurrent (Chan, ThreadId, forkIO, newChan, newEmptyMVar, readChan, readMVar, tryPutMVar, writeChan)
import Prelude hiding (log)

class Monad m => MonadDiscord m where
  restCall :: (FromJSON a, Request (r a)) => r a -> m (Either RestCallErrorCode a)
  sendCommand :: GatewaySendable -> m ()
  readCache :: m Cache
  stopDiscord :: m ()
  getEvent :: m (Either T.Text (Either GatewayException EventInternalParse))

instance {-# OVERLAPPING #-} MonadDiscord DiscordHandler where
  restCall = D.restCall
  sendCommand = D.sendCommand
  readCache = D.readCache
  stopDiscord = D.stopDiscord
  getEvent = do
    handle <- ask
    race
      (readMVar (discordHandleLibraryError handle))
      (readChan (gatewayHandleEvents (discordHandleGateway handle)))

instance MonadDiscord m => MonadDiscord (IdentityT m) where
  restCall = lift . restCall
  sendCommand = lift . sendCommand
  readCache = lift readCache
  stopDiscord = lift stopDiscord
  getEvent = lift getEvent

instance MonadDiscord m => MonadDiscord (ReaderT r m) where
  restCall = lift . restCall
  sendCommand = lift . sendCommand
  readCache = lift readCache
  stopDiscord = lift stopDiscord
  getEvent = lift getEvent

instance MonadDiscord m => MonadDiscord (StateT s m) where
  restCall = lift . restCall
  sendCommand = lift . sendCommand
  readCache = lift readCache
  stopDiscord = lift stopDiscord
  getEvent = lift getEvent

-- | Run a discord bot from the given options.
runDiscord :: RunDiscordOpts -> IO T.Text
runDiscord = runDiscordM id

-- | Run a discord bot from the given options, as well as a way to unlift the
-- MonadDiscord given. Due to the `MonadUnliftIO` constraint, this can only be
-- used on monad stacks that use the `UnliftIO` library (ReaderT, IdentityT).
--
-- Gets the handle for `runDiscordLoopM`.
runDiscordM :: forall m. (MonadUnliftIO m, MonadMask m, MonadDiscord m) => (forall a. m a -> DiscordHandler a) -> EnvRunDiscordOpts m IO -> IO T.Text
runDiscordM unlift opts = do
  log <- newChan
  logId <- liftIO $ startLogger (discordOnLog opts) log
  (cache, cacheId) <- liftIO $ startCacheThread log
  (rest, restId) <- liftIO $ startRestThread (Auth (discordToken opts)) log
  (gate, gateId) <- liftIO $ startGatewayThread (Auth (discordToken opts)) (discordGatewayIntent opts) cache log

  libE <- newEmptyMVar

  let handle =
        DiscordHandle
          { discordHandleRestChan = rest,
            discordHandleGateway = gate,
            discordHandleCache = cache,
            discordHandleLog = log,
            discordHandleLibraryError = libE,
            discordHandleThreads =
              [ HandleThreadIdLogger logId,
                HandleThreadIdRest restId,
                HandleThreadIdCache cacheId,
                HandleThreadIdGateway gateId
              ]
          }

  flip runReaderT handle . unlift $
    finally
      (runDiscordLoopM handle opts)
      (discordOnEnd opts >> stopDiscord)
  where
    startLogger :: (T.Text -> IO ()) -> Chan T.Text -> IO ThreadId
    startLogger handle logC = forkIO $
      forever $
        do
          me <- try $ readChan logC >>= toIO . handle
          case me of
            Right _ -> pure ()
            Left (_ :: IOException) ->
              -- writeChan logC "Log handler failed"
              pure ()

-- | Run a discord bot from the given options, as well as a way to unlift the
-- MonadDiscord given.
runDiscordLoopM :: forall m l. (MonadUnliftIO m, MonadMask m, MonadDiscord m) => DiscordHandle -> EnvRunDiscordOpts m l -> m T.Text
runDiscordLoopM handle opts = do
  resp <- liftIO $ writeRestCall (discordHandleRestChan handle) GetCurrentUser
  case resp of
    Left (RestCallInternalErrorCode c e1 e2) ->
      libError $
        "HTTP Error Code " <> T.pack (show c) <> " " <> TE.decodeUtf8 e1
          <> " "
          <> TE.decodeUtf8 e2
    Left (RestCallInternalHttpException e) -> libError ("HTTP Exception -  " <> T.pack (show e))
    Left (RestCallInternalNoParse _ _) -> libError "Couldn't parse GetCurrentUser"
    _ -> do
      me <- try $ discordOnStart opts
      case me of
        Left (e :: SomeException) -> libError ("discordOnStart handler stopped on an exception:\n\n" <> T.pack (show e))
        Right _ -> loop
  where
    libError :: MonadIO n => T.Text -> n T.Text
    libError msg = tryPutMVar (discordHandleLibraryError handle) msg >> pure msg

    loop :: m T.Text
    loop = do
      next <- getEvent
      case next of
        Left err -> libError err
        Right (Left err) -> libError (T.pack (show err))
        Right (Right event) -> do
          let userEvent = userFacingEvent event
          let action =
                if discordForkThreadForEvents opts
                  then void . forkIO
                  else id
          action $ do
            me <- try $ discordOnEvent opts userEvent
            case me of
              Left (e :: SomeException) ->
                writeChan
                  (discordHandleLog handle)
                  ( "eventhandler - crashed on [" <> T.pack (show userEvent) <> "] "
                      <> "          with error: "
                      <> T.pack (show e)
                  )
              Right _ -> pure ()
          loop

-- | The options for the discord bot. 
--
-- `m` is the monad stack that the bot will run in.
-- `l` is the monad that the logging operates in.
data EnvRunDiscordOpts m l = RunDiscordOpts
  { discordToken :: T.Text,
    discordOnStart :: MonadDiscord m => m (),
    discordOnEnd :: MonadDiscord m => m (),
    discordOnEvent :: MonadDiscord m => Event -> m (),
    discordOnLog :: Monad m => T.Text -> l (),
    discordForkThreadForEvents :: Bool,
    discordGatewayIntent :: GatewayIntent
  }

-- | The default options type for the discord bot.
type RunDiscordOpts = EnvRunDiscordOpts DiscordHandler IO

instance forall m l. (MonadDiscord m, Monad l) => Default (EnvRunDiscordOpts m l) where
  def =
    RunDiscordOpts
      { discordToken = "",
        discordOnStart = pure (),
        discordOnEnd = pure (),
        discordOnEvent = \_ -> pure (),
        discordOnLog = \_ -> pure (),
        discordForkThreadForEvents = True,
        discordGatewayIntent = def
      }

-- | Run a pure version of the discord bot.
--
-- This doesn't use any IO, which means it likely shouldn't be used to run a
-- real discord bot. The function given lifts the logging monad to the
-- monad stack that the bot will run in.
runDiscordMPure :: forall m l. (MonadDiscord m, MonadMask m, Monad l) => (forall a. l a -> m a) -> EnvRunDiscordOpts m l -> m T.Text
runDiscordMPure liftLog opts = do
  discordOnStart opts
  loop
  where
    doLog :: T.Text -> m T.Text
    doLog t = liftLog $ discordOnLog opts t >> pure t
    loop :: m T.Text
    loop = do
      next <- getEvent
      case next of
        Left err -> doLog err
        Right (Left err) -> doLog (T.pack (show err))
        Right (Right event) -> do
          let userEvent = userFacingEvent event
          me <- try $ discordOnEvent opts userEvent
          _ <- case me of
            Left (e :: SomeException) ->
              doLog
                ( "eventhandler - crashed on [" <> T.pack (show userEvent) <> "] "
                    <> "          with error: "
                    <> T.pack (show e)
                )
            Right _ -> pure ""
          loop
