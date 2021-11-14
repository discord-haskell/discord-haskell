{-# LANGUAGE OverloadedStrings #-}

-- | Provides a rather raw interface to the websocket events
--   through a real-time Chan
module Discord.Internal.Gateway
  ( GatewayHandle(..)
  , CacheHandle(..)
  , GatewayException(..)
  , Cache(..)
  , startCacheThread
  , startGatewayThread
  , module Discord.Internal.Types
  ) where

import Prelude hiding (log)
import Control.Concurrent.Chan (newChan, dupChan, Chan)
import Control.Concurrent (forkIO, ThreadId, newEmptyMVar, MVar)
import Data.IORef (newIORef)
import qualified Data.Text as T

import Discord.Internal.Types (Auth, Event, GatewayIntent)
import Discord.Internal.Gateway.EventLoop (connectionLoop, GatewayHandle(..), GatewayException(..))
import Discord.Internal.Gateway.Cache (cacheLoop, Cache(..), CacheHandle(..))

startCacheThread :: Chan T.Text -> IO (CacheHandle, ThreadId)
startCacheThread log = do
  events <- newChan :: IO (Chan (Either GatewayException Event))
  cache <- newEmptyMVar :: IO (MVar (Either (Cache, GatewayException) Cache))
  let cacheHandle = CacheHandle events cache
  tid <- forkIO $ cacheLoop cacheHandle log
  pure (cacheHandle, tid)

-- | Create a Chan for websockets. This creates a thread that
--   writes all the received Events to the Chan
startGatewayThread :: Auth -> GatewayIntent -> CacheHandle -> Chan T.Text -> IO (GatewayHandle, ThreadId)
startGatewayThread auth intent cacheHandle log = do
  events <- dupChan (cacheHandleEvents cacheHandle)
  sends <- newChan
  status <- newIORef Nothing
  seqid <- newIORef 0
  seshid <- newIORef ""
  let gatewayHandle = GatewayHandle events sends status seqid seshid
  tid <- forkIO $ connectionLoop auth intent gatewayHandle log
  pure (gatewayHandle, tid)



