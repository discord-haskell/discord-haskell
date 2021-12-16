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
     , cacheGuilds :: M.Map GuildId (Guild, GuildInfo)
     , cacheChannels :: M.Map ChannelId Channel
     , cacheApplication :: PartialApplication
     } deriving (Show)

data CacheHandle = CacheHandle
  { cacheHandleEvents :: Chan (Either GatewayException Event)
  , cacheHandleCache  :: MVar (Either (Cache, GatewayException) Cache)
  }

cacheLoop :: CacheHandle -> Chan T.Text -> IO ()
cacheLoop cacheHandle log = do
      ready <- readChan eventChan
      case ready of
        Right (Ready _ user dmChannels _unavailableGuilds _ _ pApp) -> do
          let dmChans = M.fromList (zip (map channelId dmChannels) dmChannels)
          putMVar cache (Right (Cache user dmChans M.empty M.empty pApp))
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

adjustCache :: Cache -> Event -> Cache
adjustCache minfo event = case event of
  --ChannelCreate Channel
  --ChannelUpdate Channel
  --ChannelDelete Channel
  GuildCreate guild info ->
    let newChans = map (setChanGuildID (guildId guild)) $ guildChannels info
        g = M.insert (guildId guild) (guild, info { guildChannels = newChans }) (cacheGuilds minfo)
        c = M.unionWith (\a _ -> a)
                        (M.fromList [ (channelId ch, ch) | ch <- newChans ])
                        (cacheChannels minfo)
    in minfo { cacheGuilds = g, cacheChannels = c }
  --GuildUpdate guild -> do
  --  let g = M.insert (guildId guild) guild (cacheGuilds minfo)
  --      m2 = minfo { cacheGuilds = g }
  --  putMVar cache m2
  --GuildDelete guild -> do
  --  let g = M.delete (guildId guild) (cacheGuilds minfo)
  --      c = M.filterWithKey (\(keyGuildId,_) _ -> keyGuildId /= guildId guild) (cacheChannels minfo)
  --      m2 = minfo { cacheGuilds = g, cacheChannels = c }
  --  putMVar cache m2
  _ -> minfo

setChanGuildID :: GuildId -> Channel -> Channel
setChanGuildID s c = if channelIsInGuild c
                     then c { channelGuild = s }
                     else c
