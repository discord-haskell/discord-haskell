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
  , RestCallException(..)
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
import Control.Exception (try, finally, IOException)
import Control.Concurrent.Chan
import Control.Concurrent.MVar
import Data.Aeson
import Data.Default (Default, def)
import qualified Data.Text as T

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

  let handle = DiscordHandle { discordRestChan = rest
                             , discordGateway = gate
                             , discordCache = cache
                             , discordThreads = [ DiscordThreadIdLogger logId
                                                , DiscordThreadIdRest restId
                                                , DiscordThreadIdCache cacheId
                                                , DiscordThreadIdGateway gateId
                                                ]
                             }

  finally (do discordOnStart opts handle
              forever $ do e <- readChan (_events_g (discordGateway handle))
                           case e of
                             Right event -> discordOnEvent opts handle event
                             Left err -> print err
          )
    (discordOnEnd opts >> stopDiscord handle)



-- | Execute one http request and get a response
restCall :: (FromJSON a, Request (r a)) =>
            DiscordHandle -> r a -> IO (Either RestCallException a)
restCall h = writeRestCall (discordRestChan h)

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

