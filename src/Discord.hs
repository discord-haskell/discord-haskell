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
  , loginRest
  , loginRestGateway
  ) where

import Control.Concurrent.Chan
import Control.Concurrent.MVar
import Data.Aeson

import Discord.Rest
import Discord.Rest.Requests
import Discord.Types
import Discord.Gateway
import Discord.Gateway.Cache

data NotLoggedIntoGateway = NotLoggedIntoGateway

loginRest :: Auth -> IO (RestChan, NotLoggedIntoGateway)
loginRest auth = do
  restHandler <- createHandler auth
  pure (restHandler, NotLoggedIntoGateway)

loginRestGateway :: Auth -> IO (RestChan, Gateway)
loginRestGateway auth = do
  restHandler  <- createHandler auth
  (chan, info) <- chanWebSocket auth
  pure (restHandler, Gateway chan info)


data Gateway = Gateway
  { _events :: Chan Event
  , _cache :: MVar Cache
  }

restCall :: FromJSON a => (RestChan, x) -> Request a -> IO (Resp a)
restCall (r,_) = writeRestCall r

nextEvent :: (RestChan, Gateway) -> IO Event
nextEvent (_,g) = readChan (_events g)

readCache :: (RestChan, Gateway) -> IO Cache
readCache (_,g) = readMVar (_cache g)

