{-# LANGUAGE RankNTypes #-}

module Discord
  ( module Discord.Types
  , module Discord.Rest.Channel
  , module Discord.Rest.Guild
  , module Discord.Rest.User
  , module Discord.Rest.Invite
  , module Discord.Rest.Emoji
  , module Discord.Rest.Voice
  , module Discord.Rest.Webhook
  , Cache(..)
  , Gateway(..)
  , RestChan(..)
  , RestCallException(..)
  , GatewayException(..)
  , Request(..)
  , ThreadIdType(..)
  , restCall
  , nextEvent
  , sendCommand
  , readCache
  , stopDiscord
  , loginRest
  , loginRestGateway
  , loginRestGatewayWithLog
  ) where

import Prelude hiding (log)
import Control.Monad (forever)
import Control.Concurrent (forkIO, threadDelay, ThreadId, killThread)
import Control.Concurrent.Chan
import Control.Concurrent.MVar
import Data.Monoid ((<>))
import Data.Aeson

import Discord.Rest
import Discord.Rest.Channel
import Discord.Rest.Guild
import Discord.Rest.User
import Discord.Rest.Invite
import Discord.Rest.Emoji
import Discord.Rest.Voice
import Discord.Rest.Webhook
import Discord.Types
import Discord.Gateway
import Discord.Gateway.Cache

-- | Thread Ids marked by what type they are
data ThreadIdType = ThreadRest ThreadId
                  | ThreadGateway ThreadId
                  | ThreadLogger ThreadId

-- | As opposed to a Gateway object
data NotLoggedIntoGateway = NotLoggedIntoGateway

-- | Start HTTP rest handler background threads
loginRest :: Auth -> IO (RestChan, NotLoggedIntoGateway, [ThreadIdType])
loginRest auth = do
  log <- newChan
  logId <- forkIO (logger log False "")
  (restHandler, restId) <- createHandler auth log
  pure (restHandler, NotLoggedIntoGateway, [ ThreadLogger logId
                                           , ThreadRest restId
                                           ])

-- | Start HTTP rest handler and gateway background threads
loginRestGateway :: Auth -> IO (RestChan, Gateway, [ThreadIdType])
loginRestGateway auth = do
  log <- newChan
  logId <- forkIO (logger log False "")
  (restHandler, restId) <- createHandler auth log
  (gate, gateId) <- startGatewayThread auth log
  pure (restHandler, gate, [ ThreadLogger logId
                           , ThreadRest restId
                           , ThreadGateway gateId
                           ])

-- | Start HTTP rest handler and gateway background threads
loginRestGatewayWithLog :: Auth -> String -> IO (RestChan, Gateway, [ThreadIdType])
loginRestGatewayWithLog auth path = do
  log <- newChan
  logId <- forkIO (logger log True path)
  (restHandler, restId) <- createHandler auth log
  (gate, gateId) <- startGatewayThread auth log
  pure (restHandler, gate, [ ThreadLogger logId
                           , ThreadRest restId
                           , ThreadGateway gateId
                           ])

-- | Execute one http request and get a response
restCall :: (FromJSON a, Request (r a)) =>
                     (RestChan, y, z) -> r a -> IO (Either RestCallException a)
restCall (r,_,_) = writeRestCall r

-- | Block until the gateway produces another event. Once an exception is returned,
--    only return that exception
nextEvent :: (x, Gateway, z) -> IO (Either GatewayException Event)
nextEvent (_,g,_) = readChan (_events g)

-- | Send a GatewaySendable, but not Heartbeat, Identify, or Resume
sendCommand :: (x, Gateway, z) -> GatewaySendable -> IO ()
sendCommand (_,g,_) e = case e of
                          Heartbeat _ -> pure ()
                          Identify _ _ _ _ -> pure ()
                          Resume _ _ _ -> pure ()
                          _ -> writeChan (_gatewayCommands g) e

-- | Access the current state of the gateway cache
readCache :: (RestChan, Gateway, z) -> IO (Either GatewayException Cache)
readCache (_,g,_) = readMVar (_cache g)

-- | Stop all the background threads
stopDiscord :: (x, y, [ThreadIdType]) -> IO ()
stopDiscord (_,_,is) = threadDelay (10^6 `div` 10) >> mapM_ (killThread . toId) is
  where toId t = case t of
                   ThreadRest a -> a
                   ThreadGateway a -> a
                   ThreadLogger a -> a

-- | Add anything from the Chan to the log file, forever
logger :: Chan String -> Bool -> String -> IO ()
logger log False _ = forever $ readChan log >>= \_ -> pure ()
logger log True  f = forever $ do
  x <- readChan log
  let line = x <> "\n\n"
  appendFile f line
