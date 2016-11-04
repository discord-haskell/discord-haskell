{-# LANGUAGE RankNTypes, ExistentialQuantification #-}
module Network.Discord.Types
  ( module Network.Discord.Types
  , module Re
  ) where
    import Control.Monad.State (StateT)

    import Control.Concurrent.STM
    import Network.WebSockets (Connection)

    import Network.Discord.Types.Json as Re
    import Network.Discord.Types.Events as Re
    import Network.Discord.Types.Gateway as Re
    import Network.Discord.Types.Prelude as Re

    class Client c where
      getAuth :: Client c => c -> Auth

    data StateEnum = Create | Start | Running | InvalidReconnect | InvalidDead

    data DiscordState = forall a. (Client a) => DiscordState {
        getState       :: StateEnum
      , getClient      :: a
      , getWebSocket   :: Connection
      , getSequenceNum :: TMVar Integer
      , getRateLimits  :: [(Int, Int)]
      }

    type DiscordM = StateT DiscordState IO
