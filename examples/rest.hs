{-# LANGUAGE OverloadedStrings #-}

import Network.Discord.Rest.HTTP
import Network.Discord.Rest.Channel
import Network.Discord.Types

import Control.Concurrent (forkIO, ThreadId, killThread)
import Control.Concurrent.MVar
import Control.Concurrent.Chan

da = DiscordAuth
        (Bot "NDUzMDU3MjkwMDMyMTE5ODA4.DfbiGA.K3K-c1Julbxs-KvbzZEu1qdkzsg")
        "0.0.1"

a :: IO ()
a = do
  c <- newChan
  id <- forkIO $ restHandler da c
  let r = CreateMessage 453207241294610444 "Hello" Nothing
  m <- newEmptyMVar
  writeChan c (r,m)
  x <- readMVar m
  print x
  killThread id
  return ()

