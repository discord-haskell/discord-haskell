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
import qualified Data.Text as T
import qualified Network.HTTP.Req as R
import qualified Data.ByteString.Char8 as Q
import Data.Ix (inRange)

import Network.Discord.Types

restHandler :: FromJson a => Chan (IO (JsonResponse a), MVar a) -> IO ()
restHandler urls = loop M.empty
  where
  loop ratelocker = do
    (action, thread) <- readChan urls
    curtime <- round <$> getPOSIXTime
    case compareRate ratelocker curtime (majorRoute action) of
      Locked -> do writeChan urls (action, thread)
                   loop ratelocker
      Available -> do (resp, timeout) <- trytillsuccess action
                      case resp of
                        Resp r -> putMVar thread (Right r)
                        BadResp r -> putMVar threaad (Left r)
                        TryAgain -> writeChan urls (action thread)
                      case timeout of
                        GlobalWait i -> threadDelay (i * 1000) >> loop ratelocker
                        PathWait i -> loop $ M.insert (majorRoute action)
                                                      (curtime + i)
                                                      ratelocker
                        NoLimit -> loop ratelocker

compareRate ratelocker curtime route =
    case M.lookup route ratelocker of
      Just unlockTime -> if curtime < unlockTime then Locked else Available
      Nothing -> Available

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
  case () of
   _ | code == 429 -> pure (RateLimited, if global then GlobalWait wait else PathWait wait)
     | code `elem` [500,502] -> pure (TryAgain, NoLimit)
     | inRange (200,299) code -> pure ( Resp resp
                                      , if remain > 0 then NoLimit else PathWait wait )
     | inRange (400,499) code -> pure ( BadResp (Q.unpack status)
                                      , if remain > 0 then NoLimit else PathWait wait )
     | otherwise -> let err = "Unknown code: " ++ show cdoe ++ " - " ++ Q.unpack status
                    in pure (BadResp err, NoLimit)

-- | The base url (Req) for API requests
baseUrl :: R.Url 'R.Https
baseUrl = R.https "discordapp.com" R./: "api" R./: apiVersion
  where apiVersion = "v6"

authHeader :: DiscordAuth -> R.Option R.Https
authHeader (DiscordAuth auth version) = R.header "Authorization" auth
                                     <> R.header "User-Agent" agent
  where
  srcUrl = "https://github.com/jano017/Discord.hs"
  agent = "DiscordBot (" <> srcUrl <> ", " <> version <> ")"

-- Append to an URL
infixl 5 //
(//) :: Show a => R.Url scheme -> a -> R.Url scheme
url // part = url /: T.pack (show part)

class DiscordRequest a where
  compileReqData :: (FromJSON r) => a -> JsonRequest r
  majorRoute     :: a -> T.Text

-- | Represtents a HTTP request made to an API that supplies a Json response
data JsonRequest r where
  Delete ::  FromJSON r                => R.Url 'R.Https      -> Option -> JsonRequest r
  Get    ::  FromJSON r                => R.Url 'R.Https      -> Option -> JsonRequest r
  Patch  :: (FromJSON r, R.HttpBody a) => R.Url 'R.Https -> a -> Option -> JsonRequest r
  Post   :: (FromJSON r, R.HttpBody a) => R.Url 'R.Https -> a -> Option -> JsonRequest r
  Put    :: (FromJSON r, R.HttpBody a) => R.Url 'R.Https -> a -> Option -> JsonRequest r

-- | Represents a range of 'Snowflake's
data Range = Range { after :: Snowflake, before :: Snowflake, limit :: Int}

-- | Convert a Range to a query string
toQueryString :: Range -> Option 'Https
toQueryString (Range a b l)
  =  "after"  =: show a
  <> "before" =: show b
  <> "limit"  =: show l

--instance (MonadIO m, DiscordRest m) => MonadHttp m where
--  handleHttpException = liftIO . throwIO

