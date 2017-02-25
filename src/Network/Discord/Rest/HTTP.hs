{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE GADTs, OverloadedStrings, InstanceSigs, TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds, ScopedTypeVariables, Rank2Types #-}
-- | Provide HTTP primitives
module Network.Discord.Rest.HTTP
  (
    fetch, Resource(..), Methods, Response, Post(..), Patch(..), Put(..), baseUrl
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
    newtype Post = Post {unPost :: forall a. ToJSON a => String -> a -> IO Response}
    newtype Put = Put {unPut :: forall a. ToJSON a => String -> a -> IO Response}
    newtype Patch = Patch {unPatch :: forall a. ToJSON a => String -> a -> IO Response}
    type Delete = String -> IO Response
    type Methods = (Get, Post, Put, Patch, Delete)
    -- see https://ghc.haskell.org/trac/ghc/wiki/ImpredicativePolymorphism for unP*

    -- | Set baseUrl, auth etc and specify body/response types
    composeMethods :: Resource -> Option -> Methods
    composeMethods res opts = (get, Post post, Put put, Patch patch, delete)
      where get item = (R.req R.GET (makeUrl item) R.NoReqBody R.lbsResponse opts)
            post item payload = p R.POST item payload
            put item payload = p R.PUT item payload
            patch item payload = p R.PATCH item payload
            p :: (R.HttpMethod m, R.HttpBodyAllowed (R.AllowsBody m) 'R.CanHaveBody, ToJSON q)
                  => m -> String -> q -> IO Response
            p m item payload = R.req m (makeUrl item) (R.ReqBodyJson payload) R.lbsResponse opts
            delete item = R.req R.DELETE (makeUrl item) R.NoReqBody R.lbsResponse opts
            makeUrl c = baseUrl R./: urlPart res R./~ (T.pack c)

    -- | Generic fetch, supplies doRequset with method implementations
    fetch :: (FromJSON b, RateLimit r) => Resource -> (Methods -> r -> IO Response) -> r -> DiscordM b
    fetch res doRequest request = do
      opts <- baseRequestOptions
      (resp, rlRem, rlNext) <- lift $ do
        let methods = composeMethods res opts
        resp <- doRequest methods request
        let parseInt = justRight . eitherDecodeStrict . fromMaybe "0" . R.responseHeader resp
        return (justRight $ eitherDecode $ R.responseBody resp,
                parseInt "X-RateLimit-Remaining", parseInt "X-RateLimit-Reset")
      when (rlRem == 0) $ setRateLimit request rlNext
      return resp

