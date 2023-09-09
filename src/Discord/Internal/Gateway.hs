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
import Control.Concurrent (forkIO, ThreadId)
import UnliftIO.STM (newTVarIO)
import Data.IORef (newIORef)
import qualified Data.Text as T
import Data.Time (getCurrentTime)

import Discord.Internal.Types (Auth, EventInternalParse, GatewayIntent)
import Discord.Internal.Gateway.EventLoop (connectionLoop, GatewayHandle(..), GatewayException(..), EventChannel)
import Discord.Internal.Gateway.Cache (cacheLoop, Cache(..), CacheHandle(..))

-- | Starts a thread for the cache
startCacheThread :: Bool -> Chan T.Text -> Chan (Either GatewayException EventInternalParse) -> IO (CacheHandle, Maybe ThreadId)
startCacheThread isEnabled log events = do
  events' <- dupChan events
  cache <- newTVarIO (error "discord-haskell: cache has not been enabled or initialised after a Ready event")
  let cacheHandle = CacheHandle cache
  mTid <- if isEnabled
    then fmap Just $ forkIO $ cacheLoop cacheHandle events' log
    else pure Nothing
  pure (cacheHandle, mTid)

-- | Create a Chan for websockets. This creates a thread that
--   writes all the received EventsInternalParse to the Chan
startGatewayThread :: Auth -> GatewayIntent -> EventChannel -> Chan T.Text -> IO (GatewayHandle, ThreadId)
startGatewayThread auth intent eventChan log = do
  events <- dupChan eventChan
  sends <- newChan
  status <- newIORef Nothing
  seqid <- newIORef 0
  seshid <- newIORef ""
  host <- newIORef "gateway.discord.gg"
  currTime <- getCurrentTime
  hbAcks <- newIORef currTime
  hbSends <- newIORef (currTime, currTime)
  let gatewayHandle = GatewayHandle events sends status seqid seshid host hbAcks hbSends
  tid <- forkIO $ connectionLoop auth intent gatewayHandle log
  pure (gatewayHandle, tid)
