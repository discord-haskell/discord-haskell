{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE GADTs, OverloadedStrings, InstanceSigs, TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds, ScopedTypeVariables, Rank2Types #-}
-- | Provide actions for User API interactions.
module Network.Discord.Rest.HTTP
  (
    fetch, Resource(..), Methods, Response
  ) where
    import Control.Monad (when)
    import Data.Maybe (fromMaybe)

    import Control.Monad.Morph (lift)
    import Data.Aeson

    import Data.Version (showVersion)

    import Data.ByteString.Char8 (pack)
    import Control.Exception (throwIO)

    import Data.Semigroup ((<>))
    import qualified Network.HTTP.Req as R
    import qualified Data.Text as T

    import qualified Control.Monad.State as St (get)

    import Network.Discord.Rest.Prelude
    import Network.Discord.Types (DiscordM, justRight, getClient, DiscordState(..), getAuth)
    import Paths_discord_hs (version)

    -- | Setup Req
    instance R.MonadHttp IO where
      handleHttpException = throwIO

    -- | The base url (Req) for API requests
    baseUrl :: R.Url 'R.Https
    baseUrl = R.https "discordapp.com" R./: "api" R./: apiVersion
      where apiVersion = "v6"

    -- | Construct base options with auth from Discord state
    baseRequestOptions :: DiscordM Option
    baseRequestOptions = do
      DiscordState {getClient=client} <- St.get
      return $ R.header "Authorization" (pack . show $ getAuth client)
            <> R.header "User-Agent" (pack $ "DiscordBot (https://github.com/jano017/Discord.hs,"
                                          ++ showVersion version ++ ")")
            <> R.header "Content-Type" "application/json" -- FIXME: I think Req appends it

    data Resource = Channel | Guild | Invite | User | Voice | Webhook
                    deriving Show
    urlPart :: Resource -> T.Text
    urlPart Channel = "channels"
    urlPart Guild = "guilds"
    urlPart Invite = "invite"
    urlPart User = "users"
    urlPart Voice = "voice"
    urlPart Webhook = "webhooks"

    type Option = R.Option 'R.Https
    type Response = R.LbsResponse
    type Get = String -> IO Response
    type Post a = String -> Maybe a -> IO Response
    type Put a = String -> Maybe a -> IO Response
    type Patch a = String -> Maybe a -> IO Response
    type Delete = String -> IO Response
    type Methods a = (Get, Post a, Put a, Patch a, Delete)

    fetch :: (FromJSON b, RateLimit r) => Resource -> (forall a . ToJSON a => Methods a -> r -> IO Response) -> r -> DiscordM b
    fetch res doRequest request = do
      opts <- baseRequestOptions
      (resp, rlRem, rlNext) <- lift $ do
        let methods = (get, post, patch, put, delete)
            get item = (R.req R.GET (makeUrl item) R.NoReqBody R.lbsResponse opts)
            post item payload = p R.POST item payload
            put item payload = p R.PUT item payload
            patch item payload = p R.PATCH item payload
            p :: (R.HttpMethod m, R.HttpBodyAllowed (R.AllowsBody m) 'R.CanHaveBody,ToJSON a) => m -> String -> Maybe a -> IO Response
            p m item Nothing = R.req m (makeUrl item) emptyJsonBody R.lbsResponse opts
            p m item (Just payload) = R.req m (makeUrl item) (R.ReqBodyJson payload) R.lbsResponse opts
            delete item = R.req R.DELETE (makeUrl item) R.NoReqBody R.lbsResponse opts
            emptyJsonBody = R.ReqBodyJson "" :: R.ReqBodyJson T.Text
            makeUrl c = baseUrl R./: urlPart res R./~ (T.pack c)
        resp <- doRequest methods request
        let parseInt = justRight . eitherDecodeStrict . fromMaybe "0" . R.responseHeader resp
        return (justRight $ eitherDecode $ R.responseBody resp,
                parseInt "X-RateLimit-Remaining", parseInt "X-RateLimit-Reset")

      when (rlRem == 0) $ setRateLimit request rlNext
      return resp

