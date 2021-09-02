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

data DiscordHandle = DiscordHandle
  { discordHandleRestChan :: RestChanHandle
  , discordHandleGateway :: GatewayHandle
  , discordHandleCache :: CacheHandle
  , discordHandleThreads :: [HandleThreadId]
  , discordHandleLog :: Chan T.Text
  , discordHandleLibraryError :: MVar T.Text
  }
