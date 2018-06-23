{-# LANGUAGE RankNTypes #-}

module Discord
  ( module Discord.Rest.Requests
  , module Discord.Types
  , Resp(..)
  , login
  , Discord(..)
  ) where

import Control.Concurrent.Chan
import Data.Aeson

import Discord.Rest
import Discord.Rest.Requests
import Discord.Types
import Discord.Gateway

data Discord = Discord
  { restCall :: forall a. FromJSON a => Request a -> IO (Resp a)
  , nextEvent :: IO Event
  }

login :: Auth -> IO Discord
login auth = do
  chan <- chanWebSocket auth
  restHandler <- createHandler auth
  pure $ Discord (writeRestCall restHandler) (readChan chan)

