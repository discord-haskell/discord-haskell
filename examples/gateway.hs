{-# LANGUAGE OverloadedStrings #-}

import Control.Concurrent (forkIO, ThreadId, killThread)
import Control.Concurrent.MVar
import Control.Concurrent.Chan
import Control.Monad
import Data.Monoid ((<>))

import Network.Discord.Gateway
import Network.Discord.Types

da = DiscordAuth
        (Bot "NDUzMDU3MjkwMDMyMTE5ODA4.DfbiGA.K3K-c1Julbxs-KvbzZEu1qdkzsg")
        "0.0.2"

a :: IO ()
a = do
  c <- newChan
  newSocket da c
  forever $ do
    x <- readChan c
    putStrLn (show x <> "\n\n")

