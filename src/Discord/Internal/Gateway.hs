-- | Provides a rather raw interface to the websocket events
--   through a real-time Chan
module Discord.Internal.Gateway
  ( DiscordHandleGateway(..)
  , DiscordHandleCache
  , GatewayException(..)
  , Cache(..)
  , startCacheThread
  , startGatewayThread
  , module Discord.Internal.Types
  ) where

import Prelude hiding (log)
import Control.Concurrent.Chan (newChan, dupChan, Chan)
import Control.Concurrent (forkIO, ThreadId, newEmptyMVar, MVar)
import Data.IORef (newIORef)
import qualified Data.Text as T

import Discord.Internal.Types (Auth, Event, GatewayIntent)
import Discord.Internal.Gateway.EventLoop (connectionLoop, DiscordHandleGateway(..), GatewayException(..))
import Discord.Internal.Gateway.Cache (cacheLoop, Cache(..), DiscordHandleCache)

startCacheThread :: Chan T.Text -> IO (DiscordHandleCache, ThreadId)
startCacheThread log = do
  events <- newChan :: IO (Chan (Either GatewayException Event))
  cache <- newEmptyMVar :: IO (MVar (Either (Cache, GatewayException) Cache))
  tid <- forkIO $ cacheLoop (events, cache) log
  pure ((events, cache), tid)

-- | Create a Chan for websockets. This creates a thread that
--   writes all the received Events to the Chan
startGatewayThread :: Auth -> GatewayIntent -> DiscordHandleCache -> Chan T.Text -> IO (DiscordHandleGateway, ThreadId)
startGatewayThread auth intent (_events, _) log = do
  events <- dupChan _events
  sends <- newChan
  status <- newIORef Nothing
  let gatewayHandle = DiscordHandleGateway events sends status
  tid <- forkIO $ connectionLoop auth intent gatewayHandle log
  pure (gatewayHandle, tid)



