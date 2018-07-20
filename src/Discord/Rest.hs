{-# LANGUAGE OverloadedStrings  #-}
{-# OPTIONS_HADDOCK prune, not-home #-}

-- | Provides a higher level interface to the rest functions.
--   Preperly writes to the rate-limit loop. Creates separate
--   MVars for each call
module Discord.Rest
  ( module Discord.Rest.Requests
  , module Discord.Types
  , Resp(..)
  , RestChan(..)
  , writeRestCall
  , createHandler
  ) where

import Data.Aeson (FromJSON, eitherDecode)
import Data.Monoid ((<>))
import Control.Concurrent.Chan
import Control.Concurrent.MVar
import Control.Concurrent (forkIO, ThreadId)
import qualified Data.ByteString.Lazy.Char8 as QL

import Discord.Types
import Discord.Rest.HTTP
import Discord.Rest.Requests


newtype RestChan = RestChan (Chan ((String, JsonRequest), MVar (Resp QL.ByteString)))

-- | Starts the http request thread. Please only call this once
createHandler :: Auth -> IO (RestChan, ThreadId)
createHandler auth = do
  c <- newChan
  tid <- forkIO $ restLoop auth c
  pure (RestChan c, tid)

-- | Execute a request blocking until a response is received
writeRestCall :: FromJSON a => RestChan -> Request a -> IO (Resp a)
writeRestCall (RestChan c) req = do
  m <- newEmptyMVar
  writeChan c (prepareRequest req, m)
  r <- readMVar m
  pure $ case eitherDecode <$> r of
    Resp (Right o) -> Resp o
    Resp (Left er) -> BadResp ("parse error - " <> er)
    NoResp        -> NoResp
    BadResp err   -> BadResp err



