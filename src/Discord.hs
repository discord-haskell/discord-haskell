{-# LANGUAGE RankNTypes #-}

module Discord
  ( module Discord.Rest.Requests
  , module Discord.Types
  , Resp(..)
  , RestPart(..)
  , Discord(..)
  , login
  ) where

import System.IO.Unsafe (unsafePerformIO)
import Control.Concurrent.Chan
import Control.Concurrent.MVar
import Data.Map.Strict as M
import Data.Aeson

import Discord.Rest
import Discord.Rest.Requests
import Discord.Types
import Discord.Gateway

newtype RestPart = RestPart { restPart :: forall a. FromJSON a => Request a -> IO (Resp a) }

data Discord = Discord
  { restCall :: RestPart
  , nextEvent :: IO Event
  -- query guild/channel info
  }

discordLogins :: MVar (M.Map Auth Discord)
discordLogins = unsafePerformIO (newMVar M.empty)
{-# NOINLINE discordLogins #-}

login :: Auth -> IO Discord
login auth = do
  logs <- takeMVar discordLogins
  case M.lookup auth logs of
    Just d -> putMVar discordLogins logs >> pure d
    Nothing -> do
      restHandler <- createHandler auth
      chan <- chanWebSocket auth
      let nextDisc = Discord (RestPart (writeRestCall restHandler)) (readChan chan)
      putMVar discordLogins (M.insert auth nextDisc logs)
      pure nextDisc
