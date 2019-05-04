{-# OPTIONS_HADDOCK prune, not-home #-}

-- | Provides a rather raw interface to the websocket events
--   through a real-time Chan
module Discord.Gateway
  ( Gateway(..)
  , GatewayException(..)
  , startGatewayThread
  , module Discord.Types
  ) where

import Prelude hiding (log)
import Control.Concurrent.Chan (newChan, dupChan, Chan)
import Control.Concurrent (forkIO, ThreadId, MVar)

import Discord.Types (Auth, Event, GatewaySendable)
import Discord.Gateway.EventLoop (connectionLoop, GatewayException(..))
import Discord.Gateway.Cache

-- | Concurrency primitives that make up the gateway. Build a higher
--   level interface over these
data Gateway = Gateway
  { _events :: Chan (Either GatewayException Event)
  , _cache :: MVar (Either GatewayException Cache)
  , _gatewayCommands :: Chan GatewaySendable
  }

-- | Create a Chan for websockets. This creates a thread that
--   writes all the received Events to the Chan
startGatewayThread :: Auth -> Chan String -> IO (Gateway, ThreadId)
startGatewayThread auth log = do
  eventsWrite <- newChan
  eventsCache <- dupChan eventsWrite
  sends <- newChan
  cache <- emptyCache :: IO (MVar (Either GatewayException Cache))
  tid <- forkIO $ connectionLoop auth eventsWrite sends log
  cacheAddEventLoopFork cache eventsCache log
  pure (Gateway eventsWrite cache sends, tid)



