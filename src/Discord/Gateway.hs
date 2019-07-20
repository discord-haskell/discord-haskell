{-# OPTIONS_HADDOCK prune, not-home #-}

-- | Provides a rather raw interface to the websocket events
--   through a real-time Chan
module Discord.Gateway
  ( DiscordGateway
  , DiscordCache
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

type DiscordCache = (Chan (Either GatewayException Event), MVar (Either GatewayException Cache))
startCacheThread :: Chan String -> IO (DiscordCache, ThreadId)
startCacheThread log = do
  events <- newChan
  cache <- emptyCache :: IO (MVar (Either GatewayException Cache))
  tid <- forkIO $ cacheLoop cache events log
  pure ((events, cache), tid)

-- | Create a Chan for websockets. This creates a thread that
--   writes all the received Events to the Chan
startGatewayThread :: Auth -> DiscordCache -> Chan String -> IO (DiscordGateway, ThreadId)
startGatewayThread auth (_events, _) log = do
  events <- dupChan _events
  sends <- newChan
  tid <- forkIO $ connectionLoop auth (events, sends) log
  pure ((events, sends), tid)



