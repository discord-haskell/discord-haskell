{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Discord
  ( module Discord.Types
  , module Discord.Rest.Channel
  , module Discord.Rest.Guild
  , module Discord.Rest.User
  , module Discord.Rest.Invite
  , module Discord.Rest.Emoji
  , module Discord.Rest.Voice
  , module Discord.Rest.Webhook
  , Cache(..)
  , DiscordGateway(..)
  , DiscordCache(..)
  , DiscordRestChan(..)
  , RestCallErrorCode(..)
  , GatewayException(..)
  , Request(..)

  , RunDiscordOpts(..)
  , DiscordHandle(..)
  , runDiscord
  , restCall
  , sendCommand
  , readCache
  ) where

import Prelude hiding (log)
import Control.Monad (forever)
import Control.Concurrent (forkIO, threadDelay, ThreadId, killThread)
import Control.Concurrent.Async (race)
import Control.Exception (try, finally, IOException)
import Control.Concurrent.Chan
import Control.Concurrent.MVar
import Data.Aeson
import Data.Default (Default, def)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

import Discord.Rest
import Discord.Rest.Channel
import Discord.Rest.Guild
import Discord.Rest.User
import Discord.Rest.Invite
import Discord.Rest.Emoji
import Discord.Rest.Voice
import Discord.Rest.Webhook
import Discord.Types
import Discord.Gateway
import Discord.Gateway.Cache


-- | Thread Ids marked by what type they are
data DiscordThreadId = DiscordThreadIdRest ThreadId
                     | DiscordThreadIdCache ThreadId
                     | DiscordThreadIdLogger ThreadId
                     | DiscordThreadIdGateway ThreadId

data DiscordHandle = DiscordHandle
  { discordRestChan :: DiscordRestChan
  , discordGateway :: DiscordGateway
  , discordCache :: DiscordCache
  , discordThreads :: [DiscordThreadId]
  , discordLog :: Chan String
  , discordLibraryError :: MVar T.Text
  }

data RunDiscordOpts = RunDiscordOpts
  { discordToken :: T.Text
  , discordOnStart :: DiscordHandle -> IO ()
  , discordOnEnd :: IO ()
  , discordOnEvent :: DiscordHandle -> Event -> IO ()
  , discordOnLog :: T.Text -> IO ()
  }

instance Default RunDiscordOpts where
  def = RunDiscordOpts { discordToken = T.pack ""
                       , discordOnStart = \_ -> pure ()
                       , discordOnEnd = pure ()
                       , discordOnEvent = \_ _-> pure ()
                       , discordOnLog = \_ -> pure ()
                       }

runDiscord :: RunDiscordOpts -> IO T.Text
runDiscord opts = do
  log <- newChan
  logId <- startLogger (discordOnLog opts) log
  (cache, cacheId) <- startCacheThread log
  (rest, restId) <- startRestThread (Auth (discordToken opts)) log
  (gate, gateId) <- startGatewayThread (Auth (discordToken opts)) cache log

  libE <- newEmptyMVar

  let handle = DiscordHandle { discordRestChan = rest
                             , discordGateway = gate
                             , discordCache = cache
                             , discordLog = log
                             , discordLibraryError = libE
                             , discordThreads = [ DiscordThreadIdLogger logId
                                                , DiscordThreadIdRest restId
                                                , DiscordThreadIdCache cacheId
                                                , DiscordThreadIdGateway gateId
                                                ]
                             }

  resp <- writeRestCall (discordRestChan handle) GetCurrentUser
  case resp of
    Left (RestCallInternalErrorCode c _ _) -> pure (T.pack ("Couldn't execute GetCurrentUser - " <> show c))
    Left (RestCallInternalHttpException e) -> pure (T.pack ("Couldn't do restcall - " <> show e))
    Left (RestCallInternalNoParse _ _) -> pure (T.pack "Couldn't parse GetCurrentUser")
    _ -> finally (do discordOnStart opts handle
                     let loop = do next <- race (readMVar (discordLibraryError handle)) (readChan (_events_g (discordGateway handle)))
                                   case next of
                                     Left libErr -> pure libErr
                                     Right e -> case e of
                                                  Right event -> discordOnEvent opts handle event >> loop
                                                  Left err -> pure (T.pack (show err))
                     loop
                 )
                 (discordOnEnd opts >> stopDiscord handle)


data RestCallErrorCode = RestCallErrorCode Int T.Text T.Text

-- | Execute one http request and get a response
restCall :: (FromJSON a, Request (r a)) =>
            DiscordHandle -> r a -> IO (Either RestCallErrorCode a)
restCall h r = do resp <- writeRestCall (discordRestChan h) r
                  case resp of
                    Right x -> pure (Right x)
                    Left (RestCallInternalErrorCode c e1 e2) ->
                      pure (Left (RestCallErrorCode c (TE.decodeUtf8 e1)
                                                      (TE.decodeUtf8 e2)))
                    Left (RestCallInternalHttpException _) -> do threadDelay (10 * 10^6)
                                                                 restCall h r
                    Left (RestCallInternalNoParse _ _) -> do _ <- tryPutMVar (discordLibraryError h) (T.pack "Parse error")
                                                             threadDelay (1 * 10^6)
                                                             restCall h r

-- | Send a GatewaySendable, but not Heartbeat, Identify, or Resume
sendCommand :: DiscordHandle -> GatewaySendable -> IO ()
sendCommand h e = case e of
                    Heartbeat _ -> pure ()
                    Identify {} -> pure ()
                    Resume {} -> pure ()
                    _ -> writeChan (_gatewayCommands (discordGateway h)) e

-- | Access the current state of the gateway cache
readCache :: DiscordHandle -> IO (Either GatewayException Cache)
readCache h = readMVar (_cache (discordCache h))


-- | Stop all the background threads
stopDiscord :: DiscordHandle -> IO ()
stopDiscord h = do threadDelay (10^6 `div` 10)
                   mapM_ (killThread . toId) (discordThreads h)
  where toId t = case t of
                   DiscordThreadIdRest a -> a
                   DiscordThreadIdGateway a -> a
                   DiscordThreadIdCache a -> a
                   DiscordThreadIdLogger a -> a

startLogger :: (T.Text -> IO ()) -> Chan String -> IO ThreadId
startLogger handle logC = forkIO $ forever $
  do me <- try $ (T.pack <$> readChan logC) >>= handle
     case me of
       Right _ -> pure ()
       Left (_ :: IOException) ->
         -- writeChan logC "Log handler failed"
         pure ()

