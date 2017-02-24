{-# LANGUAGE OverloadedStrings #-}

import Control.Monad.State
import Data.Maybe

import Network.URL
import Pipes

import Network.Discord.Types
import Network.Discord.Gateway

data PutStrClient = PsClient
instance Client PutStrClient where
  getAuth _ = Bot "TOKEN"

main :: IO ()
main = runWebsocket (fromJust $ importURL "wss://gateway.discord.gg") PsClient $ do
  st <- get
  for (eventCore (getWebSocket st))
    (\pl -> lift . liftIO $ print (pl:: Event))
