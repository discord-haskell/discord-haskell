{-# LANGUAGE OverloadedStrings  #-}
{-# OPTIONS_HADDOCK prune, not-home #-}

-- | Provides framework to interact with REST api gateways. Implementations specific to the
--   Discord API are provided in Network.Discord.Rest.Channel, Network.Discord.Rest.Guild,
--   and Network.Discord.Rest.User.
module Network.Discord.Rest
  ( module Network.Discord.Rest.Channel
  , module Network.Discord.Rest.Guild
  , module Network.Discord.Rest.User
  , restCall
  ) where

import Data.Aeson.Types
import Control.Concurrent.Chan
import Control.Concurrent.MVar
import Control.Concurrent (forkIO)

import Network.Discord.Types
import Network.Discord.Rest.HTTP
import Network.Discord.Rest.Channel
import Network.Discord.Rest.Guild
import Network.Discord.Rest.User
import Network.Discord.Rest.Prelude

restCall :: (DiscordRequest req, FromJSON a) => DiscordAuth -> req a -> IO (Resp a)
restCall auth req = do
  c <- newChan
  forkIO $ restLoop auth c
  compile c req

compile :: (DiscordRequest req, FromJSON a)
    => Chan (req a, MVar (Resp a)) -> req a -> IO (Resp a)
compile c req = do
  m <- newEmptyMVar
  writeChan c (req, m)
  readMVar m


