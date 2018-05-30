{-# LANGUAGE DataKinds, OverloadedStrings, MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes, FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE FunctionalDependencies, KindSignatures #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Utility and base types and functions for the Discord Rest API
module Network.Discord.Rest.Prelude where

import Control.Concurrent.Chan
import Control.Concurrent.MVar
import Control.Concurrent (threadDelay)
import Control.Monad (when, forever)
import Control.Exception (throwIO)

import Control.Monad.IO.Class
import Data.Monoid ((<>))
import Data.Time.Clock.POSIX
import Network.HTTP.Req (Option, Scheme(..), (=:), MonadHttp(..))
import System.Log.Logger
import qualified Data.Map.Strict as M
import qualified Data.ByteString.Char8 as Q
import Data.Ix (inRange)

import Network.Discord.Types


restHandler :: FromJson a => Chan (IO (JsonResponse a), MVar a) -> IO ()
restHandler urls = loop M.empty
  where
  loop ratelocker = do
    (action, thread) <- readChan urls
    curtime <- getCurrentTime
    case compareRate ratelocker curtime (majorRoute action) of
      Locked -> do writeChan urls (action, thread)
                   loop ratelocker
      Available -> do (resp, timeout) <- trytillsuccess action
                      case resp of
                        Resp r -> putMVar thread (Right r)
                        BadResp r -> putMVar threaad (Left r)
                        TryAgain -> writeChan urls (action thread)
                      case timeout of
                        GlobalWait i -> threadDelay (i * 10^6) >> loop ratelocker
                        PathWait i -> loop $ M.insert (majorRoute action) nextstart ratelocker
                        NoLimit -> loop ratelocker

compareRate ratelocker curtime

data RateLimited = Available | Locked

data Timeout = GlobalWait Integer
             | PathWait Integer
             | NoLimit

data Resp a = Resp a
            | BadResp String
            | TryAgain

trytillsuccess :: IO (Resp a, Timeout)
trytillsuccess action = do
  resp <- action
  let code   = R.responseStatusCode resp
      status = R.responseStatusMessage resp
      wait   = fromMaybe  2000 $ R.respsoneHeader resp "Retry-After"
      global = fromMaybe False $ R.respsoneHeader resp "X-RateLimit-Global"
      remain = fromMaybe     1 $ R.responseHeader resp "X-Ratelimit-Remaining"
  case () of _
    | code == 429 -> pure $ (RateLimited, if global then GlobalWait wait else PathWait wait)
    | code `elem` [500,502] -> pure $ (TryAgain, NoLimit)
    | inRange (200,299) code -> pure $ ( Resp resp
                                       , if remain > 0 then NoLimit else PathWait wait )
    | inRange (400,499) code -> pure $ ( BadResp (Q.unpack status)
                                       , if remain > 0 then NoLimit else PathWait wait )
    | otherwise -> (BadResp ("Unknown code: " ++ show cdoe ++ " - " ++ Q.unpack status), NoLimit)

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

-- | Represents a range of 'Snowflake's
data Range = Range { after :: Snowflake, before :: Snowflake, limit :: Int}

-- | Convert a Range to a query string
toQueryString :: Range -> Option 'Https
toQueryString (Range a b l)
  =  "after"  =: show a
  <> "before" =: show b
  <> "limit"  =: show l
