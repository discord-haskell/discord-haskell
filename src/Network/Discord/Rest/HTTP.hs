{-# LANGUAGE GADTs, OverloadedStrings, InstanceSigs, TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds, ScopedTypeVariables, Rank2Types #-}
-- | Provide HTTP primitives
module Network.Discord.Rest.HTTP
  ( JsonRequest(..)
  , R.ReqBodyJson(..)
  , R.NoReqBody(..)
  , baseUrl
  , fetch
  , makeRequest
  , (//)
  , (R./:)
  ) where

    import Data.Semigroup ((<>))

    import Control.Monad.State (get, when)
    import Data.Aeson
    import Data.ByteString.Char8 (pack, ByteString)
    import Data.Maybe (fromMaybe)
    import qualified Data.Text as T (pack)
    import qualified Network.HTTP.Req as R

    import Data.Version (showVersion)
    import Network.Discord.Rest.Prelude
    import Network.Discord.Types (DiscordM, getClient, DiscordState(..), getAuth)
    import Paths_discord_hs (version)

    -- | The base url (Req) for API requests
    baseUrl :: R.Url 'R.Https
    baseUrl = R.https "discordapp.com" R./: "api" R./: apiVersion
      where apiVersion = "v6"

    -- | Construct base options with auth from Discord state
    baseRequestOptions :: DiscordM Option
    baseRequestOptions = do
      DiscordState {getClient=client} <- get
      return $ R.header "Authorization" (pack . show $ getAuth client)
            <> R.header "User-Agent" (pack $ "DiscordBot (https://github.com/jano017/Discord.hs,"
                                          ++ showVersion version ++ ")")
    infixl 5 //
    (//) :: Show a => R.Url scheme -> a -> R.Url scheme
    url // part = url R./: (T.pack $ show part)

    type Option = R.Option 'R.Https
   
    -- | Represtents a HTTP request made to an API that supplies a Json response
    data JsonRequest r where
      Delete ::  FromJSON r                => R.Url 'R.Https      -> Option -> JsonRequest r
      Get    ::  FromJSON r                => R.Url 'R.Https      -> Option -> JsonRequest r
      Patch  :: (FromJSON r, R.HttpBody a) => R.Url 'R.Https -> a -> Option -> JsonRequest r
      Post   :: (FromJSON r, R.HttpBody a) => R.Url 'R.Https -> a -> Option -> JsonRequest r
      Put    :: (FromJSON r, R.HttpBody a) => R.Url 'R.Https -> a -> Option -> JsonRequest r

    fetch :: FromJSON r => JsonRequest r -> DiscordM (R.JsonResponse r)
    fetch (Delete url      opts) = R.req R.DELETE url R.NoReqBody R.jsonResponse =<< (<> opts) <$> baseRequestOptions
    fetch (Get    url      opts) = R.req R.GET    url R.NoReqBody R.jsonResponse =<< (<> opts) <$> baseRequestOptions
    fetch (Patch  url body opts) = R.req R.PATCH  url body        R.jsonResponse =<< (<> opts) <$> baseRequestOptions
    fetch (Post   url body opts) = R.req R.POST   url body        R.jsonResponse =<< (<> opts) <$> baseRequestOptions
    fetch (Put    url body opts) = R.req R.PUT    url body        R.jsonResponse =<< (<> opts) <$> baseRequestOptions

    makeRequest :: (RateLimit a, FromJSON r) => a -> JsonRequest r -> DiscordM r
    makeRequest req action = do
      waitRateLimit req
      resp <- fetch action
      when (parseHeader resp "X-RateLimit-Remaining" 1 < 1) $
        setRateLimit req $ parseHeader resp "X-RateLimit-Reset" 0
      return $ R.responseBody resp
      where
        parseHeader :: R.HttpResponse resp => resp -> ByteString -> Int -> Int
        parseHeader resp header def = fromMaybe def $ decodeStrict =<< R.responseHeader resp header
    
    -- | Base implementation of DoFetch, allows arbitrary HTTP requests to be performed
    instance (FromJSON r) => DoFetch (JsonRequest r) where
      doFetch req = SyncFetched . R.responseBody <$> fetch req
