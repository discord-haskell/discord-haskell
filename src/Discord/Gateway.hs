{-# OPTIONS_HADDOCK prune, not-home #-}

-- | Provides a rather raw interface to the websocket events
--   through a real-time Chan
module Discord.Gateway
  ( DiscordGateway(..)
  , DiscordCache(..)
  , GatewayException(..)
  , startCacheThread
  , startGatewayThread
  , module Discord.Types
  ) where

import Prelude hiding (log)
import Control.Concurrent.Chan (newChan, dupChan, Chan)
import Control.Concurrent (forkIO, ThreadId, MVar)

import Discord.Types (Auth, Event, GatewaySendable)
import Discord.Gateway.EventLoop (connectionLoop, GatewayException(..))
import Discord.Gateway.Cache

-- | Concurrency primitives that make up the cache
data DiscordCache = DiscordCache
  { _events_c :: Chan (Either GatewayException Event)
  , _cache :: MVar (Either GatewayException Cache)
  }

-- | Concurrency primitives that make up the gateway
data DiscordGateway = DiscordGateway
  { _events_g :: Chan (Either GatewayException Event)
  , _gatewayCommands :: Chan GatewaySendable
  }

startCacheThread :: Chan String -> IO (DiscordCache, ThreadId)
startCacheThread log = do
  events <- newChan
  cache <- emptyCache :: IO (MVar (Either GatewayException Cache))
  tid <- cacheAddEventLoopFork cache events log
  pure (DiscordCache events cache, tid)

-- | Create a Chan for websockets. This creates a thread that
--   writes all the received Events to the Chan
startGatewayThread :: Auth -> DiscordCache -> Chan String -> IO (DiscordGateway, ThreadId)
startGatewayThread auth cache log = do
  events <- dupChan (_events_c cache)
  sends <- newChan
  tid <- forkIO $ connectionLoop auth events sends log
  pure (DiscordGateway events sends, tid)



