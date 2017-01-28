{-# LANGUAGE RankNTypes, ExistentialQuantification #-}
{-# OPTIONS_HADDOCK prune, not-home #-}
module Network.Discord.Types
  ( module Network.Discord.Types
  , module Network.Discord.Types.Channel
  , module Network.Discord.Types.Guild
  , module Network.Discord.Types.Events
  , module Network.Discord.Types.Gateway
  , module Network.Discord.Types.Prelude
  ) where
    import Control.Monad.State (StateT)

    import Control.Concurrent.STM
    import Network.WebSockets (Connection)

    import Network.Discord.Types.Channel
    import Network.Discord.Types.Guild
    import Network.Discord.Types.Events
    import Network.Discord.Types.Gateway
    import Network.Discord.Types.Prelude

    class Client c where
      getAuth :: c -> Auth

    -- | A state of discord connection.
    data StateEnum = Create | Start | Running | InvalidReconnect | InvalidDead

    -- | A local state in which important connection details are stored.
    data DiscordState = forall a. (Client a) => DiscordState {
        getState       :: StateEnum
      , getClient      :: a
      , getWebSocket   :: Connection
      , getSequenceNum :: TMVar Integer
      , getRateLimits  :: [(Int, Int)]
      }

    type DiscordM = StateT DiscordState IO
