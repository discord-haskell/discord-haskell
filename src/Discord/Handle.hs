module Discord.Handle
  ( DiscordHandle(..)
  , DiscordHandleThreadId(..)
  ) where

import Control.Concurrent (ThreadId, Chan, MVar)
import qualified Data.Text as T

import Discord.Internal.Rest (DiscordHandleRestChan)
import Discord.Internal.Gateway (DiscordHandleGateway, DiscordHandleCache)

-- | Thread Ids marked by what type they are
data DiscordHandleThreadId = DiscordHandleThreadIdRest ThreadId
                           | DiscordHandleThreadIdCache ThreadId
                           | DiscordHandleThreadIdLogger ThreadId
                           | DiscordHandleThreadIdGateway ThreadId

data DiscordHandle = DiscordHandle
  { discordHandleRestChan :: DiscordHandleRestChan
  , discordHandleGateway :: DiscordHandleGateway
  , discordHandleCache :: DiscordHandleCache
  , discordHandleThreads :: [DiscordHandleThreadId]
  , discordHandleLog :: Chan String
  , discordHandleLibraryError :: MVar T.Text
  }
