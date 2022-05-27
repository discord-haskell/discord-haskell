-- | The Discord Handle. Holds all the information related to the connection.
module Discord.Handle
  ( DiscordHandle(..)
  , HandleThreadId(..)
  ) where

import Control.Concurrent (ThreadId, Chan, MVar)
import qualified Data.Text as T

import Discord.Internal.Rest (RestChanHandle(..))
import Discord.Internal.Gateway (GatewayHandle(..), CacheHandle(..))

-- | Thread Ids marked by what type they are
data HandleThreadId
  = -- | A Rest API thread
    HandleThreadIdRest ThreadId
  | -- | A cache thread
  HandleThreadIdCache ThreadId
  | -- | A logger thread
  HandleThreadIdLogger ThreadId
  | -- | A gateway thread 
  HandleThreadIdGateway ThreadId

-- | The main Handle structure
data DiscordHandle = DiscordHandle
  { -- | Handle to the Rest loop
    discordHandleRestChan :: RestChanHandle
  , -- | Handle to the Websocket gateway event loop
    discordHandleGateway :: GatewayHandle
  , -- | Handle to the cache
    discordHandleCache :: CacheHandle
  , -- | List of the threads currently in use by the library
    discordHandleThreads :: [HandleThreadId]
  , -- | `Chan` used to send messages to the internal logger
    discordHandleLog :: Chan T.Text
  , -- | `MVar` containing a description of the latest library error
    discordHandleLibraryError :: MVar T.Text
  }
