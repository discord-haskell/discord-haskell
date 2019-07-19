{-# LANGUAGE RankNTypes #-}

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
  ) where

import Prelude hiding (log)
import Control.Monad (forever)
import Control.Concurrent (forkIO, threadDelay, ThreadId, killThread)
import Control.Exception (finally)
import Control.Concurrent.Chan
import Control.Concurrent.MVar
import Data.Monoid ((<>))
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
  , discordThreads :: [ThreadIdType]
  , discordReminder :: TheseNamesAreInternalDetails
  }

data TheseNamesAreInternalDetails = TheseNamesAreInternalDetails

-- | Execute one http request and get a response
restCall :: (FromJSON a, Request (r a)) => DiscordHandle -> r a -> IO (Either RestCallException a)
restCall h = writeRestCall (discordRestChan h)

{-
-- | Send a GatewaySendable, but not Heartbeat, Identify, or Resume
sendCommand :: (x, Gateway, z) -> GatewaySendable -> IO ()
sendCommand (_,g,_) e = case e of
                          Heartbeat _ -> pure ()
                          Identify _ _ _ _ -> pure ()
                          Resume _ _ _ -> pure ()
                          _ -> writeChan (_gatewayCommands g) e

-- | Access the current state of the gateway cache
readCache :: (RestChan, Gateway, z) -> IO (Either GatewayException Cache)
readCache (_,g,_) = readMVar (_cache g)

-}

data RunDiscordOpts = RunDiscordOpts
  { discordToken :: T.Text
  , discordOnStart :: DiscordHandle -> IO ()
  , discordOnEvent :: DiscordHandle -> Event -> IO ()
  , discordOnLog :: String -> IO ()
  }

instance Default RunDiscordOpts where
  def = RunDiscordOpts { discordToken = T.pack ""
                       , discordOnStart = \_ -> pure ()
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
        , discordThreads = [ ThreadLogger logId
                           , ThreadRest restId
                           , ThreadGateway gateId
                           ]
        , discordReminder = TheseNamesAreInternalDetails
        }

  discordOnStart opts handle

  finally (forever $ do e <- readChan (_events gate)
                        case e of
                          Right event -> discordOnEvent opts handle event
                          Left err -> print err
          )
    (stopDiscord handle)


logger :: (String -> IO ()) -> Chan String -> IO ()
logger handle logC = forever $ readChan logC >>= handle

-- | Stop all the background threads
stopDiscord :: DiscordHandle -> IO ()
stopDiscord h = threadDelay (10^6 `div` 10) >> mapM_ (killThread . toId) (discordThreads h)
  where toId t = case t of
                   ThreadRest a -> a
                   ThreadGateway a -> a
                   ThreadLogger a -> a

