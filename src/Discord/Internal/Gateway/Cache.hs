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

data Cache = Cache
     { cacheCurrentUser :: !User
     , cacheDMChannels :: !(M.Map ChannelId Channel)
     , cacheGuilds :: !(M.Map GuildId (Maybe (Guild, Maybe GuildCreateData)))
     , cacheChannels :: !(M.Map ChannelId Channel)
     , cacheApplication :: !FullApplication
     } deriving (Show)

data CacheHandle = CacheHandle
  { cacheHandleEvents :: Chan (Either GatewayException EventInternalParse)
  , cacheHandleCache  :: MVar (Either (Cache, GatewayException) Cache)
  }

createCache :: User -> FullApplication -> Cache
createCache user app = Cache user M.empty M.empty M.empty app

cacheLoop :: Bool -> CacheHandle -> Chan T.Text -> IO ()
cacheLoop isEnabled cacheHandle _log = when isEnabled $ forever $ do
    eventOrExcept <- readChan eventChan
    minfo <- takeMVar cache
    case minfo of
      Left nope -> putMVar cache (Left nope)
      Right info -> case eventOrExcept of
                      Left e -> putMVar cache (Left (info, e))
                      Right event -> putMVar cache $! Right $! adjustCache info event
  where
  cache     = cacheHandleCache cacheHandle
  eventChan = cacheHandleEvents cacheHandle


adjustCache :: Cache -> EventInternalParse -> Cache
adjustCache minfo event = case event of
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
