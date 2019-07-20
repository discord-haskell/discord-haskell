{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_HADDOCK prune, not-home #-}

-- | Query info about connected Guilds and Channels
module Discord.Gateway.Cache where

import Prelude hiding (log)
import Data.Monoid ((<>))
import Control.Monad (forever)
import Control.Concurrent.MVar
import Control.Concurrent.Chan
import qualified Data.Map.Strict as M

import Discord.Types
import Discord.Gateway.EventLoop

data Cache = Cache
            { _currentUser :: User
            , _dmChannels :: M.Map ChannelId Channel
            , _guilds :: M.Map GuildId (Guild, GuildInfo)
            , _channels :: M.Map ChannelId Channel
            } deriving (Show)

type DiscordCache = (Chan (Either GatewayException Event), MVar (Either (Cache, GatewayException) Cache))

cacheLoop :: DiscordCache -> Chan String -> IO ()
cacheLoop (eventChan, cache) log = do
      ready <- readChan eventChan
      case ready of
        Right (Ready _ user dmChannels _unavailableGuilds _) -> do
          let dmChans = M.fromList (zip (map channelId dmChannels) dmChannels)
          putMVar cache (Right (Cache user dmChans M.empty M.empty))
          loop
        Right r ->
          writeChan log ("cache - stopping cache - expected Ready event, but got " <> show r)
        Left e ->
          writeChan log ("cache - stopping cache - gateway exception " <> show e)
  where
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
        g = M.insert (guildId guild) (guild, info { guildChannels = newChans }) (_guilds minfo)
        c = M.unionWith (\a _ -> a)
                        (M.fromList [ (channelId ch, ch) | ch <- newChans ])
                        (_channels minfo)
    in minfo { _guilds = g, _channels = c }
  --GuildUpdate guild -> do
  --  let g = M.insert (guildId guild) guild (_guilds minfo)
  --      m2 = minfo { _guilds = g }
  --  putMVar cache m2
  --GuildDelete guild -> do
  --  let g = M.delete (guildId guild) (_guilds minfo)
  --      c = M.filterWithKey (\(keyGuildId,_) _ -> keyGuildId /= guildId guild) (_channels minfo)
  --      m2 = minfo { _guilds = g, _channels = c }
  --  putMVar cache m2
  _ -> minfo

setChanGuildID :: GuildId -> Channel -> Channel
setChanGuildID s c = if channelIsInGuild c
                     then c { channelGuild = s }
                     else c
