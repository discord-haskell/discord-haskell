{-# LANGUAGE ExistentialQuantification, MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings, FlexibleInstances #-}
{-# OPTIONS_HADDOCK prune, not-home #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- | Provides framework to interact with REST api gateways. Implementations specific to the
--   Discord API are provided in Network.Discord.Rest.Channel, Network.Discord.Rest.Guild,
--   and Network.Discord.Rest.User.
module Network.Discord.Rest
  ( module Network.Discord.Rest
  , module Network.Discord.Rest.Prelude
  , module Network.Discord.Rest.Channel
  , module Network.Discord.Rest.Guild
  , module Network.Discord.Rest.User
  ) where
    import Data.Maybe (fromJust)

    import qualified Network.HTTP.Req as R
    import Data.Aeson.Types
    import Network.URL

    import Network.Discord.Rest.Channel
    import Network.Discord.Rest.Guild
    import Network.Discord.Rest.Prelude
    import Network.Discord.Rest.User
    import Network.Discord.Rest.HTTP (baseUrl)

    -- | Obtains a new gateway to connect to.
    getGateway :: DiscordRest m => m URL
    getGateway = do
      r <- R.req R.GET (baseUrl R./: "gateway") R.NoReqBody R.jsonResponse mempty
      return . fromJust $ importURL =<< parseMaybe getURL (R.responseBody r)
      where
        getURL :: Value -> Parser String
        getURL = withObject "url" (.: "url")
