{-# LANGUAGE OverloadedStrings  #-}
{-# OPTIONS_HADDOCK prune, not-home #-}

-- | Provides a higher level interface to the rest functions.
--   Preperly writes to the rate-limit loop. Creates separate
--   MVars for each call
module Discord.Rest
  ( module Discord.Types
  , Resp(..)
  , RestChan(..)
  , Request(..)
  , writeRestCall
  , createHandler
  ) where

import Prelude hiding (log)
import Data.Aeson (FromJSON, eitherDecode)
import Data.Monoid ((<>))
import Control.Concurrent.Chan
import Control.Concurrent.MVar
import Control.Concurrent (forkIO, ThreadId)
import qualified Data.ByteString.Lazy.Char8 as QL

import Discord.Types
import Discord.Rest.HTTP

newtype RestChan = RestChan (Chan (String, JsonRequest, MVar (Resp QL.ByteString)))

-- | Starts the http request thread. Please only call this once
createHandler :: Auth -> Chan String -> IO (RestChan, ThreadId)
createHandler auth log = do
  c <- newChan
  tid <- forkIO $ restLoop auth c log
  pure (RestChan c, tid)

-- | Execute a request blocking until a response is received
writeRestCall :: (Request (r a), FromJSON a) => RestChan -> r a -> IO (Resp a)
writeRestCall (RestChan c) req = do
  m <- newEmptyMVar
  writeChan c (majorRoute req, jsonRequest req, m)
  r <- readMVar m
  pure $ case eitherDecode <$> r of
    Resp (Right o) -> Resp o
    Resp (Left er) -> BadResp ("parse error - " <> er)
    NoResp        -> NoResp
    BadResp err   -> BadResp err



