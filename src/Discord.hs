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
  , Gateway(..)
  , RestChan(..)
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
data ThreadIdType = ThreadRest ThreadId
                  | ThreadGateway ThreadId
                  | ThreadLogger ThreadId

data DiscordHandle = DiscordHandle
  { discordRestChan :: RestChan
  , discordGateway :: Gateway
  , discordThreads :: [ThreadIdType]
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

runDiscord :: RunDiscordOpts -> IO ()
runDiscord opts = do
  log <- newChan
  logId <- forkIO (logger (discordOnLog opts) log)
  (restHandler, restId) <- createHandler (Auth (discordToken opts)) log
  (gate, gateId) <- startGatewayThread (Auth (discordToken opts)) log

  let handle = DiscordHandle
        { discordRestChan = restHandler
        , discordGateway = gate
        , discordThreads = [ ThreadLogger logId
                           , ThreadRest restId
                           , ThreadGateway gateId
                           ]
        }

  finally (do discordOnStart opts handle
              forever $ do e <- readChan (_events (discordGateway handle))
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
readCache h = readMVar (_cache (discordGateway h))


-- | Stop all the background threads
stopDiscord :: DiscordHandle -> IO ()
stopDiscord h = do threadDelay (10^6 `div` 10)
                   mapM_ (killThread . toId) (discordThreads h)
  where toId t = case t of
                   ThreadRest a -> a
                   ThreadGateway a -> a
                   ThreadLogger a -> a

logger :: (T.Text -> IO ()) -> Chan String -> IO ()
logger handle logC = forever $
  do me <- try $ (T.pack <$> readChan logC) >>= handle
     case me of
       Right _ -> pure ()
       Left (_ :: IOException) ->
         -- writeChan logC "Log handler failed"
         pure ()

