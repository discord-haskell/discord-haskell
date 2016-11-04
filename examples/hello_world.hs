{-# LANGUAGE OverloadedStrings #-}

import Data.Maybe

import Network.URL
import Pipes.Core ((+>>))

import Network.Discord.Types
import Network.Discord.Gateway
import Network.Discord.Rest

data LogClient = LClient
instance Client LogClient where
  getAuth _ = "TOKEN GOES HERE"

main :: IO ()
main = runWebsocket (fromJust $ importURL "wss://gateway.discord.gg") LClient $ do
  _ <-
    restServer +>>
      (fetch (CreateMessage "188134500411244545" "Hello, World!")
        >> fetch (CreateMessage "188134500411244545" "I'm running discord.hs!"))
  return ()
