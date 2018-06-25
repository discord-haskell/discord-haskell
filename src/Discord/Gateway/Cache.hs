{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_HADDOCK prune, not-home #-}

-- |Query info about connected Guilds and Channels
module Discord.Gateway.Cache where

import Data.List (foldl')
import Data.Monoid ((<>))
import Control.Concurrent.MVar
import Control.Concurrent.Chan
import qualified Data.Map.Strict as M

import Discord.Types

data Cache = Cache
            { _currentUser :: Maybe User
            , _dmChannels :: M.Map Snowflake Channel
            , _guilds :: M.Map Snowflake Guild
            , _channels :: M.Map (Snowflake,Snowflake) Channel
            } deriving (Show)

emptyCache :: IO (MVar Cache)
emptyCache = newMVar (Cache Nothing M.empty M.empty M.empty)

addEvent :: MVar Cache -> Chan Event -> Chan String -> IO ()
addEvent cache eventChan log = loop
  where
  loop :: IO ()
  loop = do
    event <- readChan eventChan
    --print "aft"
    --minfo <- takeMVar cache
    --writeChan log ("cache - " <> show minfo)
    --putMVar cache (adjustCache minfo event)
    loop

adjustCache :: Cache -> Event -> Cache
adjustCache minfo event = case event of
  Ready (Init _ user dmChannels guilds _) ->
    let dmChans = M.fromList (zip (map channelId dmChannels) dmChannels)
        g = M.fromList (zip (map guildId guilds) guilds)
    in Cache (Just user) dmChans g M.empty
  --ChannelCreate Channel
  --ChannelUpdate Channel
  --ChannelDelete Channel
  --GuildCreate guild -> do
  --  let g = M.insert (guildId guild) guild (_guilds minfo)
  --      c = case guildChannels guild of
  --            Nothing -> _channels minfo
  --            Just chans -> foldl' (\m (k,v) -> M.insert k v m)
  --                                 (_channels minfo)
  --                                 [ ((guildId guild, channelId ch), ch) | ch <- chans ]
  --      m2 = minfo { _guilds = g, _channels = c }
  --  putMVar cache m2
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
