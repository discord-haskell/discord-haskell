{-# OPTIONS_HADDOCK prune, not-home #-}

-- | Provides logic code for interacting with the Discord websocket
--   gateway. Reallistically, this is probably lower level than most
--   people will need, and you should use Language.Discord.
module Network.Discord.Gateway where

import Control.Concurrent (forkIO)
import Control.Concurrent.Chan
import Network.Discord.Types
import Network.Discord.Gateway.Internal

newWebSocket :: DiscordAuth -> IO (Chan Event)
newWebSocket (DiscordAuth auth _) = do
  log <- newChan
  forkIO (logger log True)
  events <- newChan
  forkIO (connectionLoop auth events log)
  pure events

