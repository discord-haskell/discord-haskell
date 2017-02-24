{-# LANGUAGE RankNTypes, ExistentialQuantification #-}
{-# OPTIONS_HADDOCK prune, not-home #-}
-- | Provides types and encoding/decoding code. Types should be identical to those provided
--   in the Discord API documentation.
module Network.Discord.Types
  ( module Network.Discord.Types
  , module Network.Discord.Types.Prelude
  , module Network.Discord.Types.Channel
  , module Network.Discord.Types.Events
  , module Network.Discord.Types.Gateway
  , module Network.Discord.Types.Guild
  ) where

    import Data.Proxy
    import Control.Monad.State (StateT)

    import Control.Concurrent.STM
    import Network.WebSockets (Connection)
    import System.IO.Unsafe (unsafePerformIO)

    import Network.Discord.Types.Channel
    import Network.Discord.Types.Events
    import Network.Discord.Types.Gateway
    import Network.Discord.Types.Guild
    import Network.Discord.Types.Prelude

    -- | Provides a list of possible states for the client gateway to be in.
    data StateEnum = Create | Start | Running | InvalidReconnect | InvalidDead

    -- | Stores details needed to manage the gateway and bot
    data DiscordState = forall a . (Client a) => DiscordState {
        getState       :: StateEnum         -- ^ Current state of the gateway
      , getClient      :: a                 -- ^ Currently running bot client
      , getWebSocket   :: Connection        -- ^ Stored WebSocket gateway
      , getSequenceNum :: TMVar Integer     -- ^ Heartbeat sequence number
      , getRateLimits  :: TVar [(Int, Int)] -- ^ List of rate-limited endpoints
      }

    -- | Convenience type alias for the monad most used throughout most Discord.hs operations
    type DiscordM = StateT DiscordState IO
    
    -- | The Client typeclass holds the majority of the user-customizable state,
    --   including merging states resulting from async operations.
    class Client c where
      -- | Provides authorization token associated with the client
      getAuth :: c -> Auth
      -- | Function for resolving state differences due to async operations.
      --   Developers are responsible for preventing race conditions.
      --   Remember as `merge newState oldState`
      merge :: c  -- ^ Modified state
            -> c  -- ^ Initial state
            -> c  -- ^ Merged state
      merge _ st = st -- By default, we simply discard the modified state (we don't
                      -- store state by default)

      -- | Control access to state. In cases where state locks aren't needed, this
      --   is most likely the best solution. This implementation most likely
      --   needs an accompanying {-\# NOINLINE getTMClient \#-} pragma to ensure
      --   that a single state is shared between events
      getTMClient :: TVar c
      getTMClient = unsafePerformIO $ newTVarIO undefined
      {-# NOINLINE getTMClient #-}

      -- | In some cases, state locks are needed to prevent race conditions or 
      --   TVars are an unwanted solution. In these cases, both getClient and
      --   modifyClient should be implemented.
      getSTMClient :: Proxy c  -- ^ Type witness for the client
        -> STM c
      getSTMClient _ = readTVar getTMClient
      
      -- | modifyClient is used by mergeClient to merge application states before
      --   and after an event handler is run
      {-# NOINLINE getSTMClient #-}
      modifyClient :: (c -> c) -> STM ()
      modifyClient f = modifyTVar getTMClient f
      
      -- | Merges application states before and after an event handler
      mergeClient :: c -> STM ()
      mergeClient client = modifyClient $ merge client
