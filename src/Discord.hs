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

import Control.Concurrent (ThreadId, killThread)
import Control.Concurrent.Chan
import Control.Concurrent.MVar
import Data.Aeson

import Discord.Rest
import Discord.Rest.Requests
import Discord.Types
import Discord.Gateway
import Discord.Gateway.Cache

loginRest :: Auth -> IO (RestChan, NotLoggedIntoGateway)
loginRest auth = do
  restHandler <- createHandler auth
  pure (restHandler, NotLoggedIntoGateway)

loginRestGateway :: Auth -> IO (RestChan, Gateway)
loginRestGateway auth = do
  restHandler  <- createHandler auth
  (chan, info, tid) <- chanWebSocket auth
  pure (restHandler, Gateway chan info tid)


data NotLoggedIntoGateway = NotLoggedIntoGateway

data Gateway = Gateway
  { _events :: Chan Event
  , _cache :: MVar Cache
  , _gatewayThreadId :: ThreadId
  }

restCall :: FromJSON a => (RestChan, x) -> Request a -> IO (Resp a)
restCall (r,_) = writeRestCall r

nextEvent :: (RestChan, Gateway) -> IO Event
nextEvent (_,g) = readChan (_events g)

readCache :: (RestChan, Gateway) -> IO Cache
readCache (_,g) = readMVar (_cache g)

stopDiscord :: KillableThread kt => (RestChan, kt) -> IO ()
stopDiscord (r,g) = stopThread r >> stopThread g

class KillableThread t where
  stopThread :: t -> IO ()

instance KillableThread NotLoggedIntoGateway where
  stopThread _ = pure ()

instance KillableThread RestChan where
  stopThread r = killThread (_restThreadId r)

instance KillableThread Gateway where
  stopThread g = killThread (_gatewayThreadId g)
logger :: Chan String -> Bool -> IO ()
logger log False = forever $ readChan log >>= \_ -> pure ()
logger log True  = forever $ do
  x <- readChan log
  let line = x <> "\n\n"
  appendFile "the-log-of-discord-haskell.txt" line
