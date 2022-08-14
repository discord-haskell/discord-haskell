{-# LANGUAGE OverloadedStrings #-}

-- | Query info about connected Guilds and Channels
module Discord.Internal.Gateway.Cache where

import Prelude hiding (log)
import Control.Monad (forever)
import Control.Concurrent.MVar
import Control.Concurrent.Chan
import qualified Data.Map.Strict as M
import qualified Data.Text as T

import Discord.Internal.Types
import Discord.Internal.Gateway.EventLoop

data Cache = Cache
     { cacheCurrentUser :: User
     , cacheDMChannels :: M.Map ChannelId Channel
     , cacheGuilds :: M.Map GuildId Guild
     , cacheChannels :: M.Map ChannelId Channel
     , cacheApplication :: PartialApplication
     } deriving (Show)

data CacheHandle = CacheHandle
  { cacheHandleEvents :: Chan (Either GatewayException EventInternalParse)
  , cacheHandleCache  :: MVar (Either (Cache, GatewayException) Cache)
  }

cacheLoop :: CacheHandle -> Chan T.Text -> IO ()
cacheLoop cacheHandle log = do
      ready <- readChan eventChan
      case ready of
        Right (InternalReady _ user _ _ _ _ pApp) -> do
          putMVar cache (Right (Cache user M.empty M.empty M.empty pApp))
          loop
        Right r ->
          writeChan log ("cache - stopping cache - expected Ready event, but got " <> T.pack (show r))
        Left e ->
          writeChan log ("cache - stopping cache - gateway exception " <> T.pack (show e))
  where
  cache     = cacheHandleCache cacheHandle
  eventChan = cacheHandleEvents cacheHandle

  loop :: IO ()
  loop = forever $ do
    eventOrExcept <- readChan eventChan
    minfo <- takeMVar cache
    case minfo of
      Left nope -> putMVar cache (Left nope)
      Right info -> case eventOrExcept of
                      Left e -> putMVar cache (Left (info, e))
                      Right event -> putMVar cache (Right (adjustCache info event))

adjustCache :: Cache -> EventInternalParse -> Cache
adjustCache minfo event = case event of
  InternalGuildCreate guild ->
    let newChans = maybe [] (map (setChanGuildID (guildId guild))) (guildChannels guild)
        g = M.insert (guildId guild) (guild { guildChannels = Just newChans }) (cacheGuilds minfo)
        c = M.unionWith const
                        (M.fromList [ (channelId ch, ch) | ch <- newChans ])
                        (cacheChannels minfo)
    in minfo { cacheGuilds = g, cacheChannels = c }
  --InternalGuildUpdate guild -> do
  --  let g = M.insert (guildId guild) guild (cacheGuilds minfo)
  --      m2 = minfo { cacheGuilds = g }
  --  putMVar cache m2
  --InternalGuildDelete guild -> do
  --  let g = M.delete (guildId guild) (cacheGuilds minfo)
  --      c = M.filterWithKey (\(keyGuildId,_) _ -> keyGuildId /= guildId guild) (cacheChannels minfo)
  --      m2 = minfo { cacheGuilds = g, cacheChannels = c }
  --  putMVar cache m2
  InternalReady _ _ _ _ _ _ pa -> minfo { cacheApplication = pa }
  _ -> minfo

setChanGuildID :: GuildId -> Channel -> Channel
setChanGuildID s c = if channelIsInGuild c
                     then c { channelGuild = s }
                     else c
