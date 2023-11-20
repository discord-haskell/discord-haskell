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
import Control.Concurrent (threadDelay, forkIO, ThreadId, newEmptyMVar, MVar)
import Data.IORef (newIORef)
import qualified Data.Text as T
import Data.Time (getCurrentTime)

import Discord.Internal.Types (Auth, EventInternalParse, GatewayIntent, DiscordSharding(..), GatewayBot(..), extractHostname)
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
startGatewayThread :: Auth -> GatewayIntent -> DiscordSharding -> GatewayBot -> CacheHandle -> Chan T.Text -> IO (GatewayHandle, [ThreadId])
startGatewayThread auth intent sharding gatewaybot cacheHandle log = do
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

  tids <- forkIO $ gatewayManagerLoop gatewayHandle
  pure (gatewayHandle, tids)

gatewayManagerLoop :: GatewayHandle -> IO ()
gatewayManagerLoop = undefined -- connectEventLoops (\shard -> connectionLoop auth shard intent gatewayHandle log) (decideSharding sharding gatewaybot)

-- | Based on user specified sharding, return the list of shards to connect
decideSharding :: DiscordSharding -> GatewayBot -> [(Int, Int)]
decideSharding sharding gatewaybot = case sharding of
                                       DiscordShardingSpecific shards -> shards
                                       DiscordShardingAuto -> let n = fromIntegral $ gatewayBotRecommendedShards gatewaybot
                                                              in [(x, n) | x <- [0..(n-1)]]

connectEventLoops :: ((Int, Int) -> IO ()) -> [(Int, Int)] -> IO [ThreadId]
connectEventLoops eventLoop shards = mapM startConnection (zip [0..] shards)
  where
  startConnection :: (Int, (Int, Int)) -> IO ThreadId
  startConnection (delay, shard) = forkIO $ threadDelay (delay * 5*10^6) >> eventLoop shard
