{-# OPTIONS_HADDOCK prune, not-home #-}

-- | Provides a rather raw interface to the websocket events
--   through a real-time Chan
module Discord.Gateway
  ( chanWebSocket
  , module Discord.Types
  ) where

import Prelude hiding (log)
import Control.Exception.Safe (finally)
import Control.Concurrent.Chan (readChan, newChan, dupChan, Chan)
import Control.Concurrent (threadDelay, forkIO, killThread, ThreadId, MVar)
import Control.Monad (forever)
import Data.Monoid ((<>))

import Discord.Types (Auth, Event)
import Discord.Gateway.EventLoop (connectionLoop)
import Discord.Gateway.Cache

-- | Create a Chan for websockets. This creates a thread that
--   writes all the received Events to the Chan
chanWebSocket :: Auth -> IO (Chan Event, MVar Cache, ThreadId)
chanWebSocket auth = do
  log <- newChan
  eventsWrite <- newChan
  eventsCache <- dupChan eventsWrite
  writeFile "the-log-of-discord-haskell.txt" ""
  logid <- forkIO (logger log True)
  cache <- emptyCache
  cacheID <- forkIO $ addEvent cache eventsCache log
  tid <- forkIO $ finally (connectionLoop auth eventsWrite log)
                          (threadDelay (2*10^6) >> killThread logid >> killThread cacheID)
  pure (eventsWrite, cache, tid)



