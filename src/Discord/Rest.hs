{-# LANGUAGE OverloadedStrings  #-}
{-# OPTIONS_HADDOCK prune, not-home #-}

-- | Provides framework to interact with REST api gateways. Implementations specific to the
--   Discord API are provided in Discord.Rest.Channel, Discord.Rest.Guild,
--   and Discord.Rest.User.
module Discord.Rest
  ( module Discord.Rest.Requests
  , module Discord.Types
  , Resp(..)
  , writeRestCall
  , createHandler
  , RestChan(..)
  ) where

import Data.Aeson
import Control.Concurrent.Chan
import Control.Concurrent.MVar
import Control.Concurrent (forkIO)
import qualified Data.ByteString.Lazy.Char8 as QL

import Discord.Types
import Discord.Rest.HTTP
import Discord.Rest.Requests


newtype RestChan = RestChan (Chan ((String, JsonRequest), MVar (Resp QL.ByteString)))

-- | Starts the http request thread. Please only call this once
createHandler :: Auth -> IO RestChan
createHandler auth = do
  c <- newChan
  forkIO $ restLoop auth c
  pure (RestChan c)

-- | Execute a request blocking until a response is recieved
writeRestCall :: FromJSON a => RestChan -> Request a -> IO (Resp a)
writeRestCall (RestChan c) req = do
  m <- newEmptyMVar
  writeChan c (prepareRequest req, m)
  r <- readMVar m
  pure $ case decode <$> r of
    Resp (Just o) -> Resp o
    Resp Nothing -> NoResp
    NoResp -> NoResp
    BadResp err -> BadResp err



