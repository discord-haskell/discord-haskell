{-# LANGUAGE OverloadedStrings #-}

-- | Provides a rather raw interface to the websocket events
--   through a real-time Chan
module Discord.Internal.Gateway
  ( GatewayHandle (..),
    CacheHandle (..),
    GatewayException (..),
    Cache (..),
    startCacheThread,
    startGatewayThread,
    module Discord.Internal.Types,
  )
where

import Control.Concurrent (MVar, ThreadId, forkIO, newEmptyMVar)
import Control.Concurrent.Chan (Chan, dupChan, newChan)
import Data.IORef (newIORef)
import qualified Data.Text as T
import Discord.Internal.Gateway.Cache (Cache (..), CacheHandle (..), cacheLoop)
import Discord.Internal.Gateway.EventLoop (GatewayException (..), GatewayHandle (..), connectionLoop)
import Discord.Internal.Types (Auth, EventInternalParse, GatewayIntent)
import Prelude hiding (log)

-- | Starts a thread for the cache
startCacheThread :: Chan T.Text -> IO (CacheHandle, ThreadId)
startCacheThread log = do
  events <- newChan :: IO (Chan (Either GatewayException EventInternalParse))
  cache <- newEmptyMVar :: IO (MVar (Either (Cache, GatewayException) Cache))
  let cacheHandle = CacheHandle events cache
  tid <- forkIO $ cacheLoop cacheHandle log
  pure (cacheHandle, tid)

-- | Create a Chan for websockets. This creates a thread that
--   writes all the received EventsInternalParse to the Chan
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
