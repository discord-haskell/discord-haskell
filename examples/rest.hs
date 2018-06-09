{-# LANGUAGE OverloadedStrings #-}

import Data.Char (isSpace)
import qualified Data.ByteString.Char8 as Q
import Network.Discord.Rest.HTTP
import Network.Discord.Rest.Channel
import Network.Discord.Types

import Control.Concurrent (forkIO, ThreadId, killThread)
import Control.Concurrent.MVar
import Control.Concurrent.Chan


a :: IO ()
a = do
  tok <- Q.filter (not . isSpace) <$> Q.readFile "./examples/auth-token.secret"
  let da = DiscordAuth (Bot tok) "0.0.4"
  c <- newChan
  id <- forkIO $ restHandler da c
  let r = -- DeleteMessage (453207241294610444, 454020395688001546)
          CreateMessage 453207241294610444 "Hello  \129302" Nothing
          -- TriggerTypingIndicator 453207241294610444
  m <- newEmptyMVar
  writeChan c (r,m)
  x <- readMVar m
  print x
  killThread id
  return ()

