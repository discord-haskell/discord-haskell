{-# LANGUAGE OverloadedStrings  #-}
{-# OPTIONS_HADDOCK prune, not-home #-}

-- | Provides framework to interact with REST api gateways. Implementations specific to the
--   Discord API are provided in Network.Discord.Rest.Channel, Network.Discord.Rest.Guild,
--   and Network.Discord.Rest.User.
module Network.Discord.Rest
  ( module Network.Discord.Rest.Requests
  , module Network.Discord.Types
--  , Rest(..)
  , Resp(..)
  , restCall
  , createHandler
  ) where

import Data.Aeson
import Control.Concurrent.Chan
import Control.Concurrent.MVar
import Control.Concurrent (forkIO)
import qualified Data.ByteString.Lazy.Char8 as QL

import Network.Discord.Types
import Network.Discord.Rest.HTTP
import Network.Discord.Rest.Requests


--newtype Rest a = Rest (Chan (Request a, MVar (Resp QL.ByteString)))

createHandler :: FromJSON a =>
    DiscordAuth -> IO (Chan (Request a, MVar (Resp QL.ByteString)))
createHandler auth = do
  c <- newChan
  forkIO $ restLoop auth c
  pure c

restCall :: FromJSON a =>
    Chan (Request a, MVar (Resp QL.ByteString)) -> Request a -> IO (Resp a)
restCall c req = do
  m <- newEmptyMVar
  writeChan c (req, m)
  r <- readMVar m
  pure $ case decode <$> r of
    Resp (Just o) -> Resp o
    Resp Nothing -> NoResp
    NoResp -> NoResp
    BadResp err -> BadResp err



