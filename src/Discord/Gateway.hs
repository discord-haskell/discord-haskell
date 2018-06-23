{-# OPTIONS_HADDOCK prune, not-home #-}

-- | Provides a rather raw interface to the websocket events
--   through a real-time Chan
module Discord.Gateway
  ( chanWebSocket
  , module Discord.Types
  ) where

import Prelude hiding (log)
import Control.Exception.Safe (finally)
import Control.Concurrent (forkIO, killThread, newChan, Chan)
import Discord.Types (Auth, Event)
import Discord.Gateway.Internal (logger, connectionLoop)

-- | Create a Chan for websockets. This creates a thread that
--   writes all the received Events to the Chan
chanWebSocket :: Auth -> IO (Chan Event)
chanWebSocket auth = do
  log <- newChan
  logid <- forkIO (logger log False)
  events <- newChan
  _ <- forkIO $ finally (connectionLoop auth events log)
                        (killThread logid)
  pure events

