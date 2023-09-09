{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

-- | Query info about connected Guilds and Channels
module Discord.Internal.Gateway.Cache where

import Prelude hiding (log)
import Control.Monad (forever, join)
import Control.Concurrent.Chan
import Data.Foldable (foldl')
import UnliftIO.STM (TVar, writeTVar, atomically, modifyTVar')
import qualified Data.Map.Strict as M
import qualified Data.Text as T

import Discord.Internal.Types
import Discord.Internal.Gateway.EventLoop

data Cache = Cache
     { cacheCurrentUser :: !User
     , cacheDMChannels :: !(M.Map ChannelId Channel)
     , cacheGuilds :: !(M.Map GuildId (Maybe (Guild, Maybe GuildCreateData)))
     , cacheChannels :: !(M.Map ChannelId Channel)
     , cacheApplication :: !PartialApplication
     } deriving (Show)

newtype CacheHandle = CacheHandle
  { cacheHandleCache  :: TVar Cache -- ^ May be an error value if the cache is disabled or the Ready event has not arrived.
  }

cacheLoop :: CacheHandle -> EventChannel -> Chan T.Text -> IO ()
cacheLoop cacheHandle eventChan log = do
      atomically $ writeTVar cache (error "discord-haskell: cache enabled, still waiting for Ready event")
      ready <- readChan eventChan
      case ready of
        Right (InternalReady _ user gids _ _ _ pApp) -> do
          let guildMap = M.fromList $ fmap (\ugid -> (idOnceAvailable ugid, Nothing)) gids
          atomically $ writeTVar cache (Cache user M.empty guildMap M.empty pApp)
          loop
        Right r ->
          writeChan log ("cache - stopping cache - expected Ready event, but got " <> T.pack (show r))
        Left e ->
          writeChan log ("cache - stopping cache - gateway exception " <> T.pack (show e))
  where
  cache     = cacheHandleCache cacheHandle

  loop :: IO ()
  loop = forever $ do
    eventOrExcept <- readChan eventChan
    atomically $
      case eventOrExcept of
        Left _ -> pure () -- nothing to do on exceptions
        Right event -> modifyTVar' cache (`adjustCache` event)

adjustCache :: Cache -> EventInternalParse -> Cache
adjustCache minfo event = case event of
  InternalReady _ _ gus _ _ _ pa -> minfo { cacheApplication = pa, cacheGuilds = M.union (cacheGuilds minfo) (M.fromList $ (\gu -> (idOnceAvailable gu, Nothing)) <$> gus) }

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
