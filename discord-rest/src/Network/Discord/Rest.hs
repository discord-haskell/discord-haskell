{-# LANGUAGE OverloadedStrings  #-}
{-# OPTIONS_HADDOCK prune, not-home #-}

-- | Provides framework to interact with REST api gateways. Implementations specific to the
--   Discord API are provided in Network.Discord.Rest.Channel, Network.Discord.Rest.Guild,
--   and Network.Discord.Rest.User.
module Network.Discord.Rest
  ( module Network.Discord.Rest.Requests
  , module Network.Discord.Types
  , restCall
  , createHandler
  ) where

import Data.Aeson.Types
import Control.Concurrent.Chan
import Control.Concurrent.MVar
import Control.Concurrent (forkIO)

import Network.Discord.Types
import Network.Discord.Rest.HTTP
import Network.Discord.Rest.Requests


newtype Rest a = Rest (Chan (Request a, MVar (Resp a)))

createHandler :: FromJSON a => DiscordAuth -> IO (Rest a)
createHandler auth = do
  c <- newChan
  forkIO $ restLoop auth c
  pure (Rest c)

restCall :: FromJSON a => Rest a -> Request a -> IO (Resp a)
restCall (Rest c) req = do
  m <- newEmptyMVar
  writeChan c (req, m)
  readMVar m


