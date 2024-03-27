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
import Control.Monad (forever, forM_)
import Control.Concurrent.Chan (newChan, dupChan, Chan)
import Control.Concurrent (threadDelay, forkIO, ThreadId, newEmptyMVar, tryPutMVar, MVar)
import Data.IORef (newIORef)
import qualified Data.Text as T
import qualified Data.Map as M
import Data.Time (getCurrentTime)
import Data.List (sort, nub)

import Discord.Internal.Types (Auth, EventInternalParse, GatewayIntent, DiscordSharding(..), GatewayBot(..), extractHostname)
import Discord.Internal.Gateway.EventLoop (connectionLoop, GatewayHandle(..), GatewayException(..))
import Discord.Internal.Gateway.Cache (cacheLoop, Cache(..), CacheHandle(..), initializeCache)

-- | Starts a thread for the cache
startCacheThread :: Bool -> GatewayEvents -> Chan T.Text -> IO (CacheHandle, ThreadId)
startCacheThread isEnabled events log = do
  cache <- newEmptyMVar :: IO (MVar Cache)
  let cacheHandle = CacheHandle events cache
  tid <- forkIO $ cacheLoop isEnabled cacheHandle log
  pure (cacheHandle, tid)


startShardManager :: Auth -> GatewayIntent -> DiscordSharding -> GatewayBot -> GatewayEvents -> Chan T.Text -> IO ShardManager
startShardManager auth intent sharding gatewaybot events log = do
  let gatewayCreate = GatewayCreate events auth intent log
  controlChan <- newChan
  let shardManager = ShardManager M.empty (0,1) controlChan gatewayCreate
  tid <- forkIO $ shardLoop gatewaybot shardManager
  writeChan controlChan (ShardManagerSetSharding sharding)
  pure (shardManager, tid)

shardLoop :: ShardManager -> [(Int, Int)] -> IO ()
shardLoop shardmanager shards = loop manager
  where
    loop manager  = do eith <- race (readChan metaRequest) (readChan shardThreadInfo)
                       case eith of
                         Left Quitting -> pure ()
                         Right shard -> do if isBlocked buckets (shardbucket shard)
                                           then writeChan shardThreadInfo shard >> loop buckets seshlimiter
                                           else pure ()

killEventLoops :: M.Map (Int, Int) GatewayHandle -> IO Int
killEventLoops loops = if M.null loops
                       then pure 0
                       else do forM_ (M.toList loops) (\h -> writeChan (gatewayHandleEvents h) (Right ) )
                               pure (5 * 10^(6 :: Int))

data ShardManager = ShardManager { -- | Currently connected gateways
                                   shardManagerGateways :: M.Map (Int, Int) GatewayHandle,
                                   -- | Primary gateway for user sendables and measuring delay
                                   shardManagerPrimaryGateway :: (Int, Int),
                                   shardManagerControl :: Chan ShardManagerMessage
                                   shardManagerGatewayCreate :: GatewayCreate
                                 }

data ShardManagerMessage = ShardManagerSetSharding DiscordSharding
                         | ShardManagerShutdown

-- | Static data. Enough info to start a Gateway Eventloop
data GatewayCreate = GatewayCreate { gatewayCreateEvents :: Chan (Either GatewayException EventInternalParse),
                                     gatewayCreateAuth :: Auth,
                                     gatewayCreateIntent :: GatewayIntent,
                                     gatewayCreateLog :: Chan T.Text
                                   }

startGatewayThread :: GatewayCreate -> (Int, Int) -> IO (GatewayHandle, ThreadId)
startGatewayThread gatewayCreate shard = do
  sends <- newChan
  status <- newIORef Nothing
  seqid <- newIORef 0
  seshid <- newIORef ""
  host <- newIORef (extractHostname (T.unpack (gatewayBotWSSUrl gatewaybot)))
  currTime <- getCurrentTime
  hbAcks <- newIORef currTime
  hbSends <- newIORef (currTime, currTime)
  let events = gatewayCreateEvents gatewayCreate
  let gatewayHandle = GatewayHandle events sends status seqid seshid host hbAcks hbSends

  let auth = gatewayCreateAuth gatewayCreate
  let intent = gatewayCreateIntent gatewayCreate
  let log = gatewayCreateLog gatewayCreate

  tid <- forkIO $ connectionLoop auth shard intent gatewayHandle log
  pure (gatewayHandle, tid)


data ShardEventLoop = ShardEventLoop { versionNumber :: Int
                                     , shardNumber :: (Int, Int)
                                     , readyToConnect :: MVar Bool
                                     }

-- | Based on user specified sharding, return the list of shards to connect.
--   Given a specific list, it will remove duplicates. If the specific list is empty, the recommended shards are used.
decideSharding :: DiscordSharding -> GatewayBot -> [(Int, Int)]
decideSharding sharding gatewaybot =
  case sharding of DiscordShardingSpecific [] -> deceideSharding DiscordShardingAuto gatewaybot
                   DiscordShardingSpecific shards -> nub shards
                   DiscordShardingAuto -> let n = fromIntegral $ gatewayBotRecommendedShards gatewaybot
                                          in [(x, n) | x <- [0..(n-1)]]
