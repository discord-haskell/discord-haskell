{-# LANGUAGE ExistentialQuantification, MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings, DataKinds #-}

-- | Utility and base types and functions for the Discord Rest API
module Network.Discord.Rest.Prelude where
  import Control.Concurrent (threadDelay)
 
  import Control.Concurrent.STM
  import Data.Aeson
  import Data.Default
  import Data.Hashable
  import Data.Monoid ((<>))
  import Data.Time.Clock.POSIX
  import Network.HTTP.Req (Option, Scheme(..), (=:))
  import System.Log.Logger
  import qualified Control.Monad.State as St

  import Network.Discord.Types

  -- | The base url for API requests
  baseURL :: String
  baseURL = "https://discordapp.com/api/v6"

  -- | Class for rate-limitable actions
  class Hashable a => RateLimit a where
    -- | Return seconds to expiration if we're waiting
    --   for a rate limit to reset
    getRateLimit  :: a -> DiscordM (Maybe Int)
    getRateLimit req = do
      DiscordState {getRateLimits=rl} <- St.get
      now <- St.liftIO (fmap round getPOSIXTime :: IO Int)
      St.liftIO . atomically $ do
        rateLimits <- readTVar rl
        case lookup (hash req) rateLimits of
          Nothing -> return Nothing
          Just a
            | a >= now  -> return $ Just a
            | otherwise -> modifyTVar' rl (delete $ hash req) >> return Nothing
    -- | Set seconds to the next rate limit reset when
    --   we hit a rate limit
    setRateLimit  :: a -> Int -> DiscordM ()
    setRateLimit req reset = do
      DiscordState {getRateLimits=rl} <- St.get
      St.liftIO . atomically . modifyTVar rl $ insert (hash req) reset
    -- | If we hit a rate limit, wait for it to reset
    waitRateLimit :: a -> DiscordM ()
    waitRateLimit endpoint = do
      rl <- getRateLimit endpoint
      case rl of
        Nothing -> return ()
        Just a  -> do
          now <- St.liftIO (fmap round getPOSIXTime :: IO Int)
          St.liftIO $ do
            infoM "Discord-hs.Rest" "Waiting for rate limit to reset..."
            threadDelay $ 1000000 * (a - now)
            putStrLn "Done"
          return ()
  
  -- | Class over which performing a data retrieval action is defined
  class DoFetch a where
    doFetch :: a -> DiscordM Fetched

  -- | Polymorphic type for all DoFetch types
  data Fetchable = forall a. (DoFetch a, Hashable a) => Fetch a

  instance DoFetch Fetchable where
    doFetch (Fetch a) = doFetch a

  instance Hashable Fetchable where
    hashWithSalt s (Fetch a) = hashWithSalt s a

  instance Eq Fetchable where
    (Fetch a) == (Fetch b) = hash a == hash b

  -- | Result of a data retrieval action
  data Fetched = forall a. (FromJSON a) => SyncFetched a
  
  -- | Represents a range of 'Snowflake's
  data Range = Range { after :: Snowflake, before :: Snowflake, limit :: Int}

  instance Default Range where
    def = Range 0 18446744073709551615 100
  
  -- | Convert a Range to a query string
  toQueryString :: Range -> Option 'Https
  toQueryString (Range a b l)
    =  "after"  =: show a 
    <> "before" =: show b 
    <> "limit"  =: show l
