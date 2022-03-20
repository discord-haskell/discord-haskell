{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module DiscordMonadTransformerLibrary
  ( runDiscord
  , restCall
  , sendCommand
  , readCache
  , stopDiscord

  , DiscordHandler

  , DiscordHandle
  , Cache(..)
  , RestCallErrorCode(..)
  , RunDiscordOpts(..)
  , FromJSON
  , def
  ) where

import Prelude hiding (log)
import Control.Monad.Reader (ReaderT, runReaderT, ask, liftIO)
import Data.Aeson (FromJSON)
import Data.Default (Default, def)
import qualified Data.Text as T

import Discord.Handle
import Discord.Internal.Rest
import Discord.Internal.Gateway

import           Discord (RestCallErrorCode(..))
import qualified Discord as DIO

type DiscordHandler = ReaderT DiscordHandle IO

data RunDiscordOpts = RunDiscordOpts
  { discordToken :: T.Text
  , discordOnStart :: DiscordHandler ()
  , discordOnEnd :: IO ()
  , discordOnEvent :: Event -> DiscordHandler ()
  , discordOnLog :: T.Text -> IO ()
  , discordForkThreadForEvents :: Bool
  , discordGatewayIntent :: GatewayIntent
  }

instance Default RunDiscordOpts where
  def = RunDiscordOpts { discordToken = ""
                       , discordOnStart = pure ()
                       , discordOnEnd = pure ()
                       , discordOnEvent = \_ -> pure ()
                       , discordOnLog = \_ -> pure ()
                       , discordForkThreadForEvents = True
                       , discordGatewayIntent = def
                       }

runDiscord :: RunDiscordOpts -> IO T.Text
runDiscord RunDiscordOpts{..} = DIO.runDiscord $ DIO.RunDiscordOpts
  { DIO.discordToken = discordToken
  , discordOnStart = \h -> runReaderT discordOnStart h
  , discordOnEnd = discordOnEnd
  , discordOnEvent = \h e -> runReaderT (discordOnEvent e) h
  , discordOnLog = discordOnLog
  , discordForkThreadForEvents = discordForkThreadForEvents
  , discordGatewayIntent = discordGatewayIntent
  }

-- | Execute one http request and get a response
restCall :: (FromJSON a, Request (r a)) => r a -> DiscordHandler (Either RestCallErrorCode a)
restCall r = do h <- ask
                liftIO $ DIO.restCall h r

-- | Send a user GatewaySendable
sendCommand :: GatewaySendable -> DiscordHandler ()
sendCommand e = do h <- ask
                   liftIO $ DIO.sendCommand h e

-- | Access the current state of the gateway cache
readCache :: DiscordHandler Cache
readCache = do h <- ask
               liftIO $ DIO.readCache h

stopDiscord :: DiscordHandler ()
stopDiscord = do h <- ask
                 liftIO $ DIO.stopDiscord h
