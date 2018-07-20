{-# LANGUAGE RankNTypes #-}

module Discord
  ( module Discord.Rest.Requests
  , module Discord.Types
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
import Discord.Rest.Requests
import Discord.Types
import Discord.Gateway
import Discord.Gateway.Cache

data ThreadIdType = ThreadRest
                  | ThreadGateway
                  | ThreadLogger

loginRest :: Auth -> IO (RestChan, NotLoggedIntoGateway, [(ThreadIdType, ThreadId)])
loginRest auth = do
  log <- newChan
  logId <- forkIO (logger log True)
  (restHandler, restId) <- createHandler auth log
  pure (restHandler, NotLoggedIntoGateway, [(ThreadLogger, logId),
                                            (ThreadRest, restId)])

loginRestGateway :: Auth -> IO (RestChan, Gateway, [(ThreadIdType, ThreadId)])
loginRestGateway auth = do
  log <- newChan
  logId <- forkIO (logger log True)
  (restHandler, restId) <- createHandler auth log
  (chan, info, gateId) <- chanWebSocket auth log
  pure (restHandler, Gateway chan info, [(ThreadLogger, logId),
                                         (ThreadRest, restId),
                                         (ThreadGateway, gateId)])


data NotLoggedIntoGateway = NotLoggedIntoGateway

data Gateway = Gateway
  { _events :: Chan Event
  , _cache :: MVar Cache
  }

restCall :: FromJSON a => (RestChan, x, y) -> Request a -> IO (Resp a)
restCall (r,_,_) = writeRestCall r

nextEvent :: (RestChan, Gateway, x) -> IO Event
nextEvent (_,g,_) = readChan (_events g)

readCache :: (RestChan, Gateway, x) -> IO Cache
readCache (_,g,_) = readMVar (_cache g)

stopDiscord :: (x, y, [(z,ThreadId)]) -> IO ()
stopDiscord (_,_,tid) = mapM_ (killThread . snd) tid

logger :: Chan String -> Bool -> IO ()
logger log False = forever $ readChan log >>= \_ -> pure ()
logger log True  = forever $ do
  x <- readChan log
  let line = x <> "\n\n"
  appendFile "the-log-of-discord-haskell.txt" line
