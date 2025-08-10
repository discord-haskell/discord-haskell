{-# LANGUAGE OverloadedStrings #-}

-- | Provides a rather raw interface to the websocket events
--   through a real-time Chan
module Discord.Internal.Gateway
  ( GatewayHandle(..)
  , CacheHandle(..)
  , GatewayException(..)
  , Cache(..)
  , initializeCache
  , startCacheThread
  , startGatewayThread
  , module Discord.Internal.Types
  ) where

import Prelude hiding (log)
import Control.Concurrent.Chan (newChan, dupChan, Chan)
import Control.Concurrent (forkIO, ThreadId, newEmptyMVar, MVar)
import Data.IORef (newIORef)
import qualified Data.Text as T
import Data.Time (getCurrentTime)

import Discord.Internal.Types (Auth, EventInternalParse, GatewayIntent, GatewayBot(..), extractHostname)
import Discord.Internal.Gateway.EventLoop (connectionLoop, GatewayHandle(..), GatewayException(..))
import Discord.Internal.Gateway.Cache (cacheLoop, Cache(..), CacheHandle(..), initializeCache)

-- | Starts a thread for the cache
startCacheThread :: Bool -> Chan T.Text -> IO (CacheHandle, ThreadId)
startCacheThread isEnabled log = do
  events <- newChan :: IO (Chan (Either GatewayException EventInternalParse))
  cache <- newEmptyMVar :: IO (MVar Cache)
  let cacheHandle = CacheHandle events cache
  tid <- forkIO $ cacheLoop isEnabled cacheHandle log
  pure (cacheHandle, tid)

-- | Create a Chan for websockets. This creates a thread that
--   writes all the received EventsInternalParse to the Chan
startGatewayThread :: Auth -> GatewayIntent -> CacheHandle -> GatewayBot -> Chan T.Text -> IO (GatewayHandle, ThreadId)
startGatewayThread auth intent cacheHandle gatewaybot log = do
  events <- dupChan (cacheHandleEvents cacheHandle)
  sends <- newChan
  status <- newIORef Nothing
  seqid <- newIORef 0
  seshid <- newIORef ""
  host <- newIORef (extractHostname (T.unpack (gatewayBotWSSUrl gatewaybot)))
  currTime <- getCurrentTime
  hbAcks <- newIORef currTime
  hbSends <- newIORef (currTime, currTime)
  let gatewayHandle = GatewayHandle events sends status seqid seshid host hbAcks hbSends
  tid <- forkIO $ connectionLoop auth intent gatewayHandle log
  pure (gatewayHandle, tid)
