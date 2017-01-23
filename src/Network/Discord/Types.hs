{-# LANGUAGE RankNTypes, ExistentialQuantification #-}
module Network.Discord.Types
  ( module Network.Discord.Types
  , module Re
  ) where
    import Control.Monad.State (StateT)

    import Control.Concurrent.STM
    import Network.WebSockets (Connection)

    import Network.Discord.Types.Channel as Re
    import Network.Discord.Types.Guild as Re
    import Network.Discord.Types.Events as Re
    import Network.Discord.Types.Gateway as Re
    import Network.Discord.Types.Prelude as Re

    class Client c where
      getAuth :: c -> Auth

    -- |A state of discord connection.
    data StateEnum = Create | Start | Running | InvalidReconnect | InvalidDead

    -- |A local state in which important connection details are stored.
    data DiscordState = forall a. (Client a) => DiscordState {
        getState       :: StateEnum
      , getClient      :: a
      , getWebSocket   :: Connection
      , getSequenceNum :: TMVar Integer
      , getRateLimits  :: [(Int, Int)]
      }

    type DiscordM = StateT DiscordState IO
