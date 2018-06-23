{-# LANGUAGE RankNTypes #-}

module Discord
  ( module Discord.Rest.Requests
  , module Discord.Types
  , Resp(..)
  , loginRest
  , loginGateway
  , Discord(..)
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

data Discord = Discord
  { restCall :: forall a. FromJSON a => Maybe (Request a -> IO (Resp a))
  , nextEvent :: Maybe (IO Event)
  }

discordLogins :: MVar (M.Map Auth Discord)
discordLogins = unsafePerformIO (newMVar M.empty)
{-# NOINLINE discordLogins #-}

loginRest :: FromJSON a => Auth -> IO (Request a -> IO (Resp a))
loginRest auth = do
  logs <- readMVar discordLogins
  case M.findWithDefault (Discord Nothing Nothing) auth logs :: Discord of
    (Discord (Just call) _) -> pure call
    (Discord  _          e) -> do
      restHandler <- createHandler auth
      let nextDisc = Discord (Just (writeRestCall restHandler)) e
      putMVar discordLogins (M.insert auth nextDisc logs)
      loginRest auth

loginGateway :: Auth -> IO (IO (Event))
loginGateway auth = do
  logs <- readMVar discordLogins
  case M.findWithDefault (Discord Nothing Nothing) auth logs of
    (Discord _ (Just e)) -> pure e
    (Discord r  Nothing) -> do
      chan <- chanWebSocket auth
      let nextDisc = Discord r (Just (readChan chan))
      putMVar discordLogins (M.insert auth nextDisc logs)
      loginGateway auth
