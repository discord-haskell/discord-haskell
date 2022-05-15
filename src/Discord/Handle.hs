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
data HandleThreadId = HandleThreadIdRest ThreadId
                      | HandleThreadIdCache ThreadId
                      | HandleThreadIdLogger ThreadId
                      | HandleThreadIdGateway ThreadId

-- | The main Handle structure
data DiscordHandle = DiscordHandle
  { discordHandleRestChan :: RestChanHandle
  , discordHandleGateway :: GatewayHandle
  , discordHandleCache :: CacheHandle
  , discordHandleThreads :: [HandleThreadId]
  , discordHandleLog :: Chan T.Text
  , discordHandleLibraryError :: MVar T.Text
  }
