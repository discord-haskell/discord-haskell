{-# OPTIONS_HADDOCK prune, not-home #-}

-- | Provides logic code for interacting with the Discord websocket
--   gateway. Reallistically, this is probably lower level than most
--   people will need, and you should use Language.Discord.
module Discord.Gateway where

import Prelude hiding (log)
import Control.Exception.Safe (finally)
import Control.Concurrent (forkIO, killThread, newChan, Chan)
import Discord.Types (Auth, Event)
import Discord.Gateway.Internal (logger, connectionLoop)

newWebSocket :: Auth -> IO (Chan Event)
newWebSocket auth = do
  log <- newChan
  logid <- forkIO (logger log False)
  events <- newChan
  _ <- forkIO $ finally (connectionLoop auth events log)
                        (killThread logid)
  pure events

