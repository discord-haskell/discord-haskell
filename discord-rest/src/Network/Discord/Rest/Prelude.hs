{-# LANGUAGE DataKinds, OverloadedStrings, MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes, FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE FunctionalDependencies, KindSignatures #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Utility and base types and functions for the Discord Rest API
module Network.Discord.Rest.Prelude where
  import Control.Concurrent (threadDelay)
  import Control.Monad (when)
  import Control.Exception (throwIO)

  import Control.Monad.IO.Class
  import Data.Default
  import Data.Hashable
  import Data.Monoid ((<>))
  import Data.Time.Clock.POSIX
  import Network.HTTP.Req (Option, Scheme(..), (=:), MonadHttp(..))
  import System.Log.Logger

  import Network.Discord.Types

  -- | The base url for API requests
  baseURL :: String
  baseURL = "https://discordapp.com/api/v6"

  class (MonadIO m, DiscordAuth m) => DiscordRest m where
    getRateLimit  :: DoFetch f a => f a -> m (Maybe Int)

    setRateLimit  :: DoFetch f a => f a -> Int -> m ()

    waitRateLimit :: DoFetch f a => f a -> m ()
    waitRateLimit endpoint = do
      rl <- getRateLimit endpoint
      case rl of
        Nothing -> return ()
        Just l -> do
          now <- liftIO (fmap round getPOSIXTime :: IO Int)
          when (l > now) . liftIO $ do
            infoM "Discord-hs.Rest" "Hit rate limit, backing off"
            threadDelay $ 1000000 * (l - now)
            infoM "Discord-hs.Rest" "Done waiting"
          return ()

  instance (MonadIO m, DiscordRest m) => MonadHttp m where
    handleHttpException = liftIO . throwIO

  -- | Class over which performing a data retrieval action is defined
  class Hashable (a b) => DoFetch (a :: * -> *) b | a b -> b where
    doFetch :: DiscordRest m => a b -> m b
  
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
