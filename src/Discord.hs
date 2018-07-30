{-# LANGUAGE RankNTypes #-}

module Discord
  ( module Discord.Types
  , module Discord.Rest.Channel
  , module Discord.Rest.Guild
  , module Discord.Rest.User
  , Resp(..)
  , Cache(..)
  , Gateway(..)
  , RestChan(..)
  , restCall
  , nextEvent
  , readCache
  , stopDiscord
  , loginRest
  , loginRestGateway
  ) where

import Prelude hiding (log)
import Control.Monad (forever)
import Control.Concurrent (forkIO, threadDelay, ThreadId, killThread)
import Control.Concurrent.Chan
import Control.Concurrent.MVar
import Data.Monoid ((<>))
import Data.Aeson

import Discord.Rest
import Discord.Rest.Channel
import Discord.Rest.Guild
import Discord.Rest.User
import Discord.Types
import Discord.Gateway
import Discord.Gateway.Cache

-- | Thread Ids marked by what type they are
data ThreadIdType = ThreadRest ThreadId
                  | ThreadGateway ThreadId
                  | ThreadLogger ThreadId

data NotLoggedIntoGateway = NotLoggedIntoGateway

-- | Start HTTP rest handler background threads
loginRest :: Auth -> IO (RestChan, NotLoggedIntoGateway, [ThreadIdType])
loginRest auth = do
  log <- newChan
  logId <- forkIO (logger log True)
  (restHandler, restId) <- createHandler auth log
  pure (restHandler, NotLoggedIntoGateway, [ ThreadLogger logId
                                           , ThreadRest restId
                                           ])

-- | Start HTTP rest handler and gateway background threads
loginRestGateway :: Auth -> IO (RestChan, Gateway, [ThreadIdType])
loginRestGateway auth = do
  log <- newChan
  logId <- forkIO (logger log True)
  (restHandler, restId) <- createHandler auth log
  (chan, info, gateId) <- chanWebSocket auth log
  pure (restHandler, Gateway chan info, [ ThreadLogger logId
                                        , ThreadRest restId
                                        , ThreadGateway gateId
                                        ])

-- | Concurrency primitives that make up the gateway. Build a higher
--   level interface over these
data Gateway = Gateway
  { _events :: Chan Event
  , _cache :: MVar Cache
  }

-- | Execute one http request and get a response
restCall :: (FromJSON a, Request (r a)) => (RestChan, x, y) -> r a -> IO (Resp a)
restCall (r,_,_) = writeRestCall r

-- | Block until the gateway produces another event
nextEvent :: (RestChan, Gateway, x) -> IO Event
nextEvent (_,g,_) = readChan (_events g)

-- | Access the current state of the gateway cache
readCache :: (RestChan, Gateway, x) -> IO Cache
readCache (_,g,_) = readMVar (_cache g)

-- | Stop all the background threads
stopDiscord :: (x, y, [ThreadIdType]) -> IO ()
stopDiscord (_,_,is) = threadDelay (10^6 `div` 10) >> mapM_ (killThread . toId) is
  where toId t = case t of
                   ThreadRest a -> a
                   ThreadGateway a -> a
                   ThreadLogger a -> a

-- | Add anything from the Chan to the log file, forever
logger :: Chan String -> Bool -> IO ()
logger log False = forever $ readChan log >>= \_ -> pure ()
logger log True  = forever $ do
  x <- readChan log
  let line = x <> "\n\n"
  appendFile "the-log-of-discord-haskell.txt" line
