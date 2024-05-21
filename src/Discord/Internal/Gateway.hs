{-# LANGUAGE OverloadedStrings #-}

-- | Provides a rather raw interface to the websocket events
--   through a real-time Chan
module Discord.Internal.Gateway
  ( GatewayHandle(..)
  , ShardManager(..)
  , CacheHandle(..)
  , GatewayException(..)
  , Cache(..)
  , initializeCache
  , startCacheThread
  , startShardManager
  , module Discord.Internal.Types
  ) where

import Prelude hiding (log)
import Control.Monad (forM_)
import Control.Concurrent.Chan (newChan, Chan, writeChan, readChan)
import Control.Concurrent (threadDelay, forkIO, ThreadId, newEmptyMVar, MVar)
import Network.WebSockets (ConnectionException(CloseRequest))
import Data.Time.Clock.POSIX (POSIXTime, getPOSIXTime)
import qualified Data.Text as T
import Data.IORef
import qualified Data.Map as M
import Data.Time (getCurrentTime)
import Data.List (sort, nub)

import Discord.Internal.Types (Auth, EventInternalParse, GatewayIntent, DiscordSharding(..), GatewayBot(..), extractHostname)
import Discord.Internal.Gateway.EventLoop (connectionLoop, GatewayHandle(..), GatewayException(..))
import Discord.Internal.Gateway.Cache (cacheLoop, Cache(..), CacheHandle(..), initializeCache)

type GatewayEvents = Chan (Either GatewayException EventInternalParse)

-- | Starts a thread for the cache
startCacheThread :: Bool -> GatewayEvents -> Chan T.Text -> IO (CacheHandle, ThreadId)
startCacheThread isEnabled events log = do
  cache <- newEmptyMVar :: IO (MVar Cache)
  let cacheHandle = CacheHandle events cache
  tid <- forkIO $ cacheLoop isEnabled cacheHandle log
  pure (cacheHandle, tid)

startShardManager :: Auth -> GatewayIntent -> GatewayEvents -> Chan T.Text -> IO ShardManager
startShardManager auth intent events log = do
  controlChan <- newChan
  gates <- newIORef M.empty
  buckets <- newIORef M.empty
  let gatewayCreate = GatewayCreate events auth controlChan intent log
  let shardManager = ShardManager gates controlChan buckets gatewayCreate
  _ <- forkIO $ shardLoop shardManager
  pure shardManager

shardLoop :: ShardManager -> IO ()
shardLoop manager = loop
  where
    loop = do eith <- readChan (shardManagerControl manager)
              case eith of
                Left ShardManagerShutdown -> do gates <- readIORef (shardManagerGateways manager)
                                                killEventLoops gates
                                                pure ()
                Left (ShardManagerSetSharding sharding gatebot) -> do
                  let shards = decideSharding sharding gatebot
                  gates <- readIORef (shardManagerGateways manager)
                  if sort shards == sort (M.keys gates)
                  then loop
                  else do killEventLoops gates
                          newGates <- startEventLoops (shardManagerGatewayCreate manager) (gatewayBotWSSUrl gatebot) shards
                          writeIORef (shardManagerGateways manager) newGates
                Right shard -> do if isBlocked (shardManagerRateLimitUnlockTimes manager) shard
                                  then do -- let shard know its ok to connect
                                          loop
                                  else do now <- getPOSIXTime
                                          modifyIORef (shardManagerRateLimitUnlockTimes manager) (setShard shard (now+5))
                                          loop


isBlocked = undefined
setShard = undefined

startEventLoops :: GatewayCreate -> T.Text -> [(Int, Int)] -> IO (M.Map (Int, Int) GatewayHandle)
startEventLoops create wssurl shards = fmap M.fromList $ mapM (\s -> fmap ((s,)) (startGatewayThread create s wssurl)) shards

-- | Kills the threads for gateway event loops
killEventLoops :: M.Map (Int, Int) GatewayHandle -> IO ()
killEventLoops loops = if M.null loops
                       then pure ()
                       else do forM_ (M.elems loops) (\h -> writeIORef (gatewayHandleShouldClose h) True)
                               pure ()

data ShardManager = ShardManager { -- | Currently connected gateways
                                   shardManagerGateways :: IORef (M.Map (Int, Int) GatewayHandle),
                                   shardManagerControl :: Chan (Either ShardManagerMessage Int),
                                   shardManagerRateLimitUnlockTimes :: IORef (M.Map Int POSIXTime),
                                   shardManagerGatewayCreate :: GatewayCreate
                                 }

data ShardManagerMessage = ShardManagerSetSharding DiscordSharding GatewayBot
                         | ShardManagerShutdown

-- | Static data. Enough info to start a Gateway Eventloop
data GatewayCreate = GatewayCreate { gatewayCreateEvents :: Chan (Either GatewayException EventInternalParse),
                                     gatewayCreateAuth :: Auth,
                                     gatewayCreateRateLimitChan :: Chan (Either ShardManagerMessage Int),
                                     gatewayCreateIntent :: GatewayIntent,
                                     gatewayCreateLog :: Chan T.Text
                                   }

startGatewayThread :: GatewayCreate -> (Int, Int) -> T.Text -> IO GatewayHandle
startGatewayThread gatewayCreate shard wssurl = do
  sends <- newChan
  status <- newIORef Nothing
  seqid <- newIORef 0
  seshid <- newIORef ""
  host <- newIORef (extractHostname (T.unpack wssurl))
  currTime <- getCurrentTime
  shouldClose <- newIORef False
  hbAcks <- newIORef currTime
  hbSends <- newIORef (currTime, currTime)
  let events = gatewayCreateEvents gatewayCreate
  let ratelimit = gatewayCreateRateLimitChan gatewayCreate
  let gatewayHandle = GatewayHandle events sends status seqid seshid host ratelimit shouldClose hbAcks hbSends

  let auth = gatewayCreateAuth gatewayCreate
  let intent = gatewayCreateIntent gatewayCreate
  let log = gatewayCreateLog gatewayCreate

  _ <- forkIO $ connectionLoop auth shard intent gatewayHandle log
  -- TODO: write tid to handle?
  pure gatewayHandle


--data ShardEventLoop = ShardEventLoop { versionNumber :: Int
--                                     , shardNumber :: (Int, Int)
--                                     , readyToConnect :: MVar Bool
--                                     }

-- | Based on user specified sharding, return the list of shards to connect.
--   Given a specific list, it will remove duplicates. If the specific list is empty, the recommended shards are used.
decideSharding :: DiscordSharding -> GatewayBot -> [(Int, Int)]
decideSharding sharding gatewaybot =
  case sharding of DiscordShardingSpecific [] -> decideSharding DiscordShardingAuto gatewaybot
                   DiscordShardingSpecific shards -> nub shards
                   DiscordShardingAuto -> let n = fromIntegral $ gatewayBotRecommendedShards gatewaybot
                                          in [(x, n) | x <- [0..(n-1)]]
