{-# LANGUAGE RankNTypes #-}

module Discord
  ( module Discord.Rest.Requests
  , module Discord.Types
  , Resp(..)
  , login
  , Discord(..)
  , Mode(..)
  ) where

import Control.Concurrent.Chan
import Data.Aeson

import Discord.Rest
import Data.HashMap.Strict (empty)
import Discord.Rest.Requests
import Discord.Types
import Discord.Gateway

data Discord = Discord
  { restCall :: forall a. FromJSON a => Request a -> IO (Resp a)
  , nextEvent :: IO Event
  }

data Mode = Rest | Gateway | RestGateway

login :: Auth -> Mode -> IO Discord
login auth m = case m of
  Rest -> do
    restHandler <- createHandler auth
    let gate = UnknownEvent "Change mode to RestGateway for this" empty
    pure $ Discord (writeRestCall restHandler) (pure gate)
  Gateway -> do
    chan <- chanWebSocket auth
    pure $ Discord (\_ -> pure NoResp) (readChan chan)
  RestGateway -> do
    chan <- chanWebSocket auth
    restHandler <- createHandler auth
    pure $ Discord (writeRestCall restHandler) (readChan chan)

