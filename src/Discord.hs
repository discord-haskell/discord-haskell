{-# LANGUAGE RankNTypes #-}

module Discord
  ( module Discord.Rest.Requests
  , module Discord.Types
  , Resp(..)
  , RestPart(..)
  , loginRest
  , loginGateway
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
  { restCall :: Maybe RestPart
  , nextEvent :: Maybe (IO Event)
  }

discordLogins :: MVar (M.Map Auth Discord)
discordLogins = unsafePerformIO (newMVar M.empty)
{-# NOINLINE discordLogins #-}

loginRest :: Auth -> IO RestPart
loginRest auth = do
  logs <- takeMVar discordLogins
  case M.findWithDefault (Discord Nothing Nothing) auth logs of
    (Discord (Just call) _) -> putMVar discordLogins logs >> pure call
    (Discord  _          e) -> do
      restHandler <- createHandler auth
      let nextDisc = Discord (Just (RestPart (writeRestCall restHandler))) e
      putMVar discordLogins (M.insert auth nextDisc logs)
      loginRest auth

loginGateway :: Auth -> IO (IO (Event))
loginGateway auth = do
  logs <- takeMVar discordLogins
  case M.findWithDefault (Discord Nothing Nothing) auth logs of
    (Discord _ (Just e)) -> putMVar discordLogins logs >> pure e
    (Discord r  _      ) -> do
      chan <- chanWebSocket auth
      let nextDisc = Discord r (Just (readChan chan))
      putMVar discordLogins (M.insert auth nextDisc logs)
      loginGateway auth
