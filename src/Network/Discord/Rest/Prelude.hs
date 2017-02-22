{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE ExistentialQuantification, MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings, FlexibleInstances, DataKinds #-}

-- | Utility and base types and functions for the Discord Rest API
module Network.Discord.Rest.Prelude where
  import Data.ByteString (append)
  import Data.ByteString.Char8 (pack)
  import Data.Default
  import Data.Time.Clock.POSIX
  import Control.Concurrent (threadDelay)

  import Data.Aeson
  import Control.Exception (throwIO)
  import qualified Network.HTTP.Req as R
  import Data.Semigroup ((<>))
  import Data.Hashable
  import qualified Control.Monad.State as St
  import Paths_discord_hs (version)
  import Data.Version (showVersion)

  import Network.Discord.Types

  instance R.MonadHttp IO where
    handleHttpException = throwIO

  -- | Read function specialized for Integers
  readInteger :: String -> Integer
  readInteger = read

  baseUrl :: R.Url 'R.Https
  baseUrl = R.https "discordapp.com" R./: "api" R./: apiVersion
    where apiVersion = "v6"

  baseURL :: String
  baseURL = "https://discordapp.com/api/v6"

  -- | Construct base options with auth from Discord state
  baseRequestOptions :: DiscordM (R.Option 'R.Https)
  baseRequestOptions = do
    DiscordState {getClient=client} <- St.get
    return $ R.header "Authorization" (append "Bot " . pack $ getAuth client)
          <> R.header "User-Agent" (pack $ "DiscordBot (https://github.com/jano017/Discord.hs,"
                                        ++ showVersion version ++ ")")
          <> R.header "Content-Type" "application/json"

  -- | Class for rate-limitable actions
  class RateLimit a where
    -- | Return seconds to expiration if we're waiting
    --   for a rate limit to reset
    getRateLimit  :: a -> DiscordM (Maybe Int)
    -- | Set seconds to the next rate limit reset when
    --   we hit a rate limit
    setRateLimit  :: a -> Int -> DiscordM ()
    -- | If we hit a rate limit, wait for it to reset
    waitRateLimit :: a -> DiscordM ()
    waitRateLimit endpoint = do
      rl <- getRateLimit endpoint
      case rl of
        Nothing -> return ()
        Just a  -> do
          now <- St.liftIO (fmap round getPOSIXTime :: IO Int)
          St.liftIO $ do
            putStrLn "Waiting for rate limit to reset..."
            threadDelay $ 1000000 * (a - now)
            putStrLn "Done"
          return ()

  class DoFetch a where
    doFetch :: a -> DiscordM Fetched

  data Fetchable = forall a. (DoFetch a, Hashable a) => Fetch a

  instance DoFetch Fetchable where
    doFetch (Fetch a) = doFetch a

  instance Hashable Fetchable where
    hashWithSalt s (Fetch a) = hashWithSalt s a

  instance Eq Fetchable where
    (Fetch a) == (Fetch b) = hash a == hash b

  data Fetched = forall a. (FromJSON a) => SyncFetched a

  data Range = Range { after :: Snowflake, before :: Snowflake, limit :: Snowflake}

  instance Default Range where
    def = Range "0" "18446744073709551615" "100"

  toQueryString :: Range -> String
  toQueryString (Range a b l) = "after="++a++"&before="++b++"&limit="++l
