-- | Provides cord Discord functionallity. 
module Network.Discord
  (
    module Network.Discord.Rest
  , module Network.Discord.Types
  , module Network.Discord.Gateway
  , module Control.Monad.State
  , module Pipes
  , module Pipes.Core
  ) where
    import Network.Discord.Rest
    import Network.Discord.Types
    import Network.Discord.Gateway
    import Pipes ((~>), (>->), yield, await)
    import Pipes.Core ((+>>))
    import Control.Monad.State (get, put, liftIO, void, when, unless)
