{-# LANGUAGE ExistentialQuantification, MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings, FlexibleInstances #-}
module Network.Discord.Rest
  (
    module Network.Discord.Rest
  , module Rest
  ) where
    import Pipes.Core
    import Control.Monad.Morph (lift)
    import Data.Hashable

    import Network.Discord.Types
    import Network.Discord.Rest.Prelude as Rest
    import Network.Discord.Rest.Channel as Rest

    restServer :: Fetchable -> Server Fetchable Fetched DiscordM Fetched
    restServer req =
      lift (doFetch req) >>= respond >>= restServer

    fetch :: (DoFetch a, Hashable a)
      => a -> Pipes.Core.Client Fetchable Fetched DiscordM Fetched
    fetch req = request $ Fetch req
