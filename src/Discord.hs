{-# LANGUAGE RankNTypes #-}

module Discord
  ( module Discord.Rest.Requests
  , module Discord.Types
  , Resp(..)
  , Discord(..)
  , Cache(..)
  , RestPart(..)
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

newtype RestPart = RestPart { rest :: forall a. FromJSON a => Request a -> IO (Resp a) }

data Discord = Discord
  { _discordRest :: RestChan
  , _discordEvents :: Chan Event
  , _discordCache :: MVar Cache
  }

restCall :: FromJSON a => Discord -> Request a -> IO (Resp a)
restCall d = writeRestCall (_discordRest d)

nextEvent :: Discord -> IO Event
nextEvent d = readChan (_discordEvents d)

readCache :: Discord -> IO Cache
readCache d = readMVar (_discordCache d)


loginRest :: Auth -> IO RestPart
loginRest auth = do
  restHandler <- createHandler auth
  pure (RestPart (writeRestCall restHandler))

loginRestGateway :: Auth -> IO Discord
loginRestGateway auth = do
  restHandler <- createHandler auth
  (chan,info) <- chanWebSocket auth
  pure (Discord restHandler chan info)
