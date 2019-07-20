{-# LANGUAGE OverloadedStrings  #-}
{-# OPTIONS_HADDOCK prune, not-home #-}

-- | Provides a higher level interface to the rest functions.
--   Preperly writes to the rate-limit loop. Creates separate
--   MVars for each call
module Discord.Rest
  ( module Discord.Types
  , DiscordRestChan
  , Request(..)
  , writeRestCall
  , startRestThread
  , RestCallInternalException(..)
  ) where

import Prelude hiding (log)
import Data.Aeson (FromJSON, eitherDecode)
import Control.Concurrent.Chan
import Control.Concurrent.MVar
import Control.Concurrent (forkIO, ThreadId)
import qualified Data.ByteString.Lazy.Char8 as QL

import Discord.Types
import Discord.Rest.HTTP

type DiscordRestChan = Chan (String, JsonRequest, MVar (Either RestCallInternalException QL.ByteString))

-- | Starts the http request thread. Please only call this once
startRestThread :: Auth -> Chan String -> IO (DiscordRestChan, ThreadId)
startRestThread auth log = do
  c <- newChan
  tid <- forkIO $ restLoop auth c log
  pure (c, tid)

-- | Execute a request blocking until a response is received
writeRestCall :: (Request (r a), FromJSON a) => DiscordRestChan -> r a -> IO (Either RestCallInternalException a)
writeRestCall c req = do
  m <- newEmptyMVar
  writeChan c (majorRoute req, jsonRequest req, m)
  r <- readMVar m
  pure $ case eitherDecode <$> r of
    Right (Right o) -> Right o
    Right (Left er) -> Left (RestCallInternalNoParse er (case r of Right x -> x
                                                                   Left _ -> ""))
    Left e -> Left e


