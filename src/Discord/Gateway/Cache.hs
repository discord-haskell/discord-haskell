{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_HADDOCK prune, not-home #-}

-- |Query info about connected Guilds and Channels
module Discord.Gateway.Cache where

import Prelude hiding (log)
import Data.Monoid ((<>))
import Control.Concurrent.MVar
import Control.Concurrent.Chan
import qualified Data.Map.Strict as M

import Discord.Types

data Cache = Cache
            { _currentUser :: User
            , _dmChannels :: M.Map Snowflake Channel
            , _guilds :: M.Map Snowflake Guild
            , _channels :: M.Map Snowflake Channel
            } deriving (Show)

emptyCache :: IO (MVar Cache)
emptyCache = newEmptyMVar

addEvent :: MVar Cache -> Chan Event -> Chan String -> IO ()
addEvent cache eventChan log = do
      ready <- readChan eventChan
      case ready of
        (Ready _ user dmChannels _unavailableGuilds _) -> do
          let dmChans = M.fromList (zip (map channelId dmChannels) dmChannels)
          putMVar cache (Cache user dmChans M.empty M.empty)
          loop
        _ -> do
          writeChan log ("Cache error - expected Ready, but got " <> show ready)
          addEvent cache eventChan log
  where
  loop :: IO ()
  loop = do
    event <- readChan eventChan
    minfo <- takeMVar cache
    writeChan log ("cache - " <> show (adjustCache minfo event))
    putMVar cache (adjustCache minfo event)
    loop

adjustCache :: Cache -> Event -> Cache
adjustCache minfo event = case event of
  --ChannelCreate Channel
  --ChannelUpdate Channel
  --ChannelDelete Channel
  GuildCreate guild ->
    let newChans = map (setChanGuildID (guildId guild)) $ guildChannels guild
        g = M.insert (guildId guild) (guild { guildChannels = newChans }) (_guilds minfo)
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

setChanGuildID :: Snowflake -> Channel -> Channel
setChanGuildID s c = if isGuildChannel c
                     then c { channelGuild = s }
                     else c
