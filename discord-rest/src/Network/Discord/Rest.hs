{-# LANGUAGE OverloadedStrings  #-}
{-# OPTIONS_HADDOCK prune, not-home #-}

-- | Provides framework to interact with REST api gateways. Implementations specific to the
--   Discord API are provided in Network.Discord.Rest.Channel, Network.Discord.Rest.Guild,
--   and Network.Discord.Rest.User.
module Network.Discord.Rest
  ( module Network.Discord.Rest.Requests
  , module Network.Discord.Types
  , Resp(..)
  , restCall
  , createHandler
  , RestChan(..)
  ) where

import Data.Aeson
import Control.Concurrent.Chan
import Control.Concurrent.MVar
import Control.Concurrent (forkIO)
import qualified Data.ByteString.Lazy.Char8 as QL

import Network.Discord.Types
import Network.Discord.Rest.HTTP
import Network.Discord.Rest.Requests


newtype RestChan = RestChan (Chan ((String, JsonRequest), MVar (Resp QL.ByteString)))

-- | Starts the http request thread. Please only call this once
createHandler :: DiscordAuth -> IO RestChan
createHandler auth = do
  c <- newChan
  forkIO $ restLoop auth c
  pure (RestChan c)

-- | Execute a request blocking until a response is recieved
restCall :: FromJSON a => RestChan -> Request a -> IO (Resp a)
restCall (RestChan c) req = do
  m <- newEmptyMVar
  writeChan c (prepareRequest req, m)
  r <- readMVar m
  pure $ case decode <$> r of
    Resp (Just o) -> Resp o
    Resp Nothing -> NoResp
    NoResp -> NoResp
    BadResp err -> BadResp err



