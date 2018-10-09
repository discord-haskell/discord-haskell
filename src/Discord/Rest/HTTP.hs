{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE MultiWayIf #-}

-- | Provide HTTP primitives
module Discord.Rest.HTTP
  ( restLoop
  , Request(..)
  , JsonRequest(..)
  ) where

import Prelude hiding (log)
import Data.Semigroup ((<>))

import Control.Monad.IO.Class (liftIO)
import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar
import Control.Concurrent.Chan
import Data.Ix (inRange)
import Data.Time.Clock.POSIX (POSIXTime, getPOSIXTime)
import qualified Data.ByteString.Char8 as Q
import qualified Data.ByteString.Lazy.Char8 as QL
import Data.Maybe (fromMaybe)
import Text.Read (readMaybe)
import qualified Network.HTTP.Req as R
import qualified Data.Map.Strict as M

import Discord.Types
import Discord.Rest.Prelude

restLoop :: Auth -> Chan (String, JsonRequest, MVar (Either (Int, Q.ByteString) QL.ByteString)) -> Chan String -> IO ()
restLoop auth urls log = loop M.empty
  where
  loop ratelocker = do
    threadDelay (40 * 1000)
    (route, request, thread) <- readChan urls
    curtime <- getPOSIXTime
    case compareRate ratelocker route curtime of
      Locked -> do writeChan urls (route, request, thread)
                   loop ratelocker
      Available -> do let action = compileRequest auth request
                      (resp, retry) <- restIOtoIO (tryRequest action log)
                      writeChan log ("rest - response " <> show resp)
                      case resp of
                        ResponseTryAgain -> writeChan urls (route, request, thread)
                        ResponseByteString "" -> putMVar thread (Right "[]") -- decode "[]" == () for
                        ResponseByteString bs -> putMVar thread (Right bs)   --      expected empty calls
                        ResponseErrorCode e s -> putMVar thread (Left (e,s))
                      case retry of
                        GlobalWait i -> do
                            writeChan log ("rest - GLOBAL WAIT LIMIT: " <> show ((i - curtime) * 1000))
                            threadDelay $ round ((i - curtime + 0.1) * 1000)
                            loop ratelocker
                        PathWait i -> loop $ M.insert route i ratelocker
                        NoLimit -> loop ratelocker

compareRate :: (Ord k, Ord v) => M.Map k v -> k -> v -> RateLimited
compareRate ratelocker route curtime =
    case M.lookup route ratelocker of
      Just unlockTime -> if curtime < unlockTime then Locked else Available
      Nothing -> Available

data RateLimited = Available | Locked

data Timeout = GlobalWait POSIXTime
             | PathWait POSIXTime
             | NoLimit


data RequestResponse = ResponseTryAgain
                     | ResponseByteString QL.ByteString
                     | ResponseErrorCode Int Q.ByteString
    deriving (Show)

tryRequest :: RestIO R.LbsResponse -> Chan String -> RestIO (RequestResponse, Timeout)
tryRequest action log = do
  resp <- action
  next10 <- liftIO (round . (+10) <$> getPOSIXTime)
  let code   = R.responseStatusCode resp
      status = R.responseStatusMessage resp
      remain = fromMaybe 1 $ readMaybeBS =<< R.responseHeader resp "X-Ratelimit-Remaining"
      global = fromMaybe False $ readMaybeBS =<< R.responseHeader resp "X-RateLimit-Global"
      resetInt = fromMaybe next10 $ readMaybeBS =<< R.responseHeader resp "X-RateLimit-Reset"
      reset  = fromIntegral resetInt
  if | code == 429 -> do liftIO $ writeChan log ("rest - 429 RATE LIMITED global:"
                                                 <> show global <> " reset:" <> show reset)
                         pure (ResponseTryAgain, if global then GlobalWait reset
                                                           else PathWait reset)
     | code `elem` [500,502] -> pure (ResponseTryAgain, NoLimit)
     | inRange (200,299) code -> pure ( ResponseByteString (R.responseBody resp)
                                      , if remain > 0 then NoLimit else PathWait reset )
     | inRange (400,499) code -> pure (ResponseErrorCode code status
                                      , if remain > 0 then NoLimit else PathWait reset )
     | otherwise -> pure (ResponseErrorCode code status, NoLimit)

readMaybeBS :: Read a => Q.ByteString -> Maybe a
readMaybeBS = readMaybe . Q.unpack

compileRequest :: Auth -> JsonRequest -> RestIO R.LbsResponse
compileRequest auth request = action
  where
  authopt = authHeader auth
  action = case request of
    (Delete url      opts) -> R.req R.DELETE url R.NoReqBody R.lbsResponse (authopt <> opts)
    (Get    url      opts) -> R.req R.GET    url R.NoReqBody R.lbsResponse (authopt <> opts)
    (Patch  url body opts) -> R.req R.PATCH  url body        R.lbsResponse (authopt <> opts)
    (Put    url body opts) -> R.req R.PUT    url body        R.lbsResponse (authopt <> opts)
    (Post   url body opts) -> do b <- body
                                 R.req R.POST   url b        R.lbsResponse (authopt <> opts)

