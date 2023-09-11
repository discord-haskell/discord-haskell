{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

-- | Query info about connected Guilds and Channels
module Discord.Internal.Gateway.Cache where

import Prelude hiding (log)
import Control.Monad (forever, join, when)
import Control.Concurrent.MVar
import Control.Concurrent.Chan
import Data.Foldable (foldl')
import qualified Data.Map.Strict as M
import qualified Data.Text as T

import Discord.Internal.Types
import Discord.Internal.Gateway.EventLoop

-- |  Cached data from gateway. Set RunDiscordOpts.discordEnableCache=true to enable all the fields
data Cache = Cache
     { cacheCurrentUser :: !User -- ^ Filled before onStart handler
     , cacheDMChannels :: !(M.Map ChannelId Channel) -- ^ Filled over time
     , cacheGuilds :: !(M.Map GuildId (Maybe (Guild, Maybe GuildCreateData))) -- ^ Filled over time
     , cacheChannels :: !(M.Map ChannelId Channel) -- ^ Filled over time
     , cacheApplication :: !FullApplication -- ^ Filled before onStart handler
     } deriving (Show)

-- | Internal handle for cacheLoop to manage the cache
data CacheHandle = CacheHandle
  { cacheHandleEvents :: Chan (Either GatewayException EventInternalParse) -- ^ Read gateway events
  , cacheHandleCache  :: MVar Cache -- ^ Current cache.
  }

-- | Internally used to setup the first cache
initializeCache :: User -> FullApplication -> CacheHandle -> IO ()
initializeCache user app cacheHandle = putMVar (cacheHandleCache cacheHandle) (Cache user M.empty M.empty M.empty app)

-- | IO loop to update cache on each gateway event
cacheLoop :: Bool -> CacheHandle -> Chan T.Text -> IO ()
cacheLoop isEnabled cacheHandle _log = when isEnabled $ forever $ do
    eventOrExcept <- readChan (cacheHandleEvents cacheHandle)
    case eventOrExcept of
      Left _ -> pure ()
      Right event -> modifyMVar_ (cacheHandleCache cacheHandle) $! pure . adjustCache event

-- | Apply gateway event to cache
adjustCache :: EventInternalParse -> Cache -> Cache
adjustCache event minfo = case event of
  -- note: ready only sends a partial app. we could update the info stored in the full app
  InternalReady _ _ gus _ _ _ _partialApp -> minfo { cacheGuilds = M.union (cacheGuilds minfo) (M.fromList $ (\gu -> (idOnceAvailable gu, Nothing)) <$> gus) }

  InternalGuildCreate guild guildData ->
    let newChans = guildCreateChannels guildData
        g = M.insert (guildId guild) (Just (guild, Just guildData)) (cacheGuilds minfo)
        c = M.union
              (M.fromList [ (channelId ch, ch) | ch <- newChans ])
              (cacheChannels minfo)
    in minfo { cacheGuilds = g, cacheChannels = c }
  InternalGuildUpdate guild ->
    let gs = M.alter (\case Just (Just (_, mCD)) -> Just (Just (guild, mCD)) ; _ -> Just (Just (guild, Nothing)); ) (guildId guild) $ cacheGuilds minfo
    in minfo { cacheGuilds = gs }
  InternalGuildDelete guild ->
    let
      toDelete = join $ cacheGuilds minfo M.!? idOnceAvailable guild
      extraData = snd =<< toDelete
      channels = maybe [] (fmap channelId . guildCreateChannels) extraData
      g = M.delete (idOnceAvailable guild) (cacheGuilds minfo)
      c = foldl' (flip M.delete) (cacheChannels minfo) channels
    in minfo { cacheGuilds = g, cacheChannels = c }
  InternalChannelCreate c ->
    let cm = M.insert (channelId c) c (cacheChannels minfo)
    in minfo { cacheChannels = cm }
  InternalChannelUpdate c ->
    let cm = M.insert (channelId c) c (cacheChannels minfo)
    in minfo { cacheChannels = cm }
  InternalChannelDelete c ->
    let cm = M.delete (channelId c) (cacheChannels minfo)
    in minfo { cacheChannels = cm }
  _ -> minfo
