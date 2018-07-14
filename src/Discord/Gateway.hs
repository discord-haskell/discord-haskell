{-# OPTIONS_HADDOCK prune, not-home #-}

-- | Provides a rather raw interface to the websocket events
--   through a real-time Chan
module Discord.Gateway
  ( chanWebSocket
  , module Discord.Types
  ) where

import Prelude hiding (log)
import Control.Exception.Safe (finally)
import Control.Concurrent (threadDelay, forkIO, killThread, readChan, newChan, dupChan, Chan, MVar)
import Control.Monad (forever)
import Data.Monoid ((<>))

import Discord.Types (Auth, Event)
import Discord.Gateway.EventLoop (connectionLoop)
import Discord.Gateway.Cache

-- | Create a Chan for websockets. This creates a thread that
--   writes all the received Events to the Chan
chanWebSocket :: Auth -> IO (Chan Event, MVar Cache)
chanWebSocket auth = do
  log <- newChan
  eventsWrite <- newChan
  eventsCache <- dupChan eventsWrite
  logid <- forkIO (logger log False)
  cache <- emptyCache
  cacheID <- forkIO $ addEvent cache eventsCache log
  _ <- forkIO $ finally (connectionLoop auth eventsWrite log)
                        (threadDelay (2*10^6) >> killThread logid >> killThread cacheID)
  pure (eventsWrite, cache)


logger :: Chan String -> Bool -> IO ()
logger log True  = forever $ readChan log >>= putStrLn . ((<>) "\n")
logger log False = forever $ readChan log >>= \_ -> pure ()
