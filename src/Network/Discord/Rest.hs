{-# LANGUAGE ExistentialQuantification, MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings, FlexibleInstances #-}
module Network.Discord.Rest
  (
    module Network.Discord.Rest
  , module Rest
  ) where
    import Pipes.Core
    import Control.Monad (void)
    import Control.Monad.Morph (lift)
    import Data.Hashable
    import Data.Maybe (fromJust)

    import Network.Discord.Types as Dc
    import Network.URL
    import Network.Wreq
    import Control.Lens
    import Data.Aeson.Types
    import Network.Discord.Rest.Prelude as Rest
    import Network.Discord.Rest.Channel as Rest
    import Network.Discord.Rest.Guild   as Rest
    import Network.Discord.Rest.User    as Rest

    restServer :: Fetchable -> Server Fetchable Fetched DiscordM Fetched
    restServer req =
      lift (doFetch req) >>= respond >>= restServer

    fetch :: (DoFetch a, Hashable a)
      => a -> Pipes.Core.Proxy X () c' c DiscordM Fetched
    fetch req = restServer +>> (request $ Fetch req)
    
    fetch' :: (DoFetch a, Hashable a)
      => a -> Pipes.Core.Proxy X () c' c DiscordM ()
    fetch' = void . fetch

    withApi :: Pipes.Core.Client Fetchable Fetched DiscordM Fetched
      -> Effect DiscordM ()
    withApi inner = void $ restServer +>> inner

    -- |Obtains a new gateway to connect to.
    getGateway :: IO URL
    getGateway = do
      resp <- asValue =<< get (baseURL++"/gateway")
      return . fromJust $ importURL =<< parseMaybe getURL (resp ^. responseBody)
      where
        getURL :: Value -> Parser String
        getURL = withObject "url" (.: "url")
