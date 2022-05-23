{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE MultiWayIf #-}

-- | Provide HTTP primitives
module Discord.Internal.Rest.HTTP
  ( restLoop
  , Request(..)
  , JsonRequest(..)
  , RestCallInternalException(..)
  ) where

import Prelude hiding (log)

import Control.Monad.IO.Class (liftIO)
import Control.Concurrent (threadDelay)
import Control.Exception.Safe (try)
import Control.Concurrent.MVar
import Control.Concurrent.Chan
import Data.Ix (inRange)
import Data.Time.Clock.POSIX (POSIXTime, getPOSIXTime)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Text.Read (readMaybe)
import Data.Maybe (fromMaybe)
import qualified Network.HTTP.Req as R
import qualified Data.Map.Strict as M

import Discord.Internal.Types
import Discord.Internal.Rest.Prelude

-- | An exception in a Rest call
data RestCallInternalException
  -- | Error code from Discord
  = RestCallInternalErrorCode Int B.ByteString B.ByteString
  -- | Couldn't parse the response
  | RestCallInternalNoParse String BL.ByteString
  -- | Something went bad in the HTTP process
  | RestCallInternalHttpException R.HttpException
  deriving (Show)

-- | Rest event loop
restLoop :: Auth -> Chan (String, JsonRequest, MVar (Either RestCallInternalException BL.ByteString))
                 -> Chan T.Text -> IO ()
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
                      reqIO <- try $ restIOtoIO (tryRequest log action)
                      case reqIO :: Either R.HttpException (RequestResponse, Timeout) of
                        Left e -> do
                          writeChan log ("rest - http exception " <> T.pack (show e))
                          putMVar thread (Left (RestCallInternalHttpException e))
                          loop ratelocker
                        Right (resp, retry) -> do
                          case resp of
                            -- decode "[]" == () for expected empty calls
                            ResponseByteString "" -> putMVar thread (Right "[]")
                            ResponseByteString bs -> putMVar thread (Right bs)
                            ResponseErrorCode e s b ->
                              putMVar thread (Left (RestCallInternalErrorCode e s b))
                            ResponseTryAgain -> writeChan urls (route, request, thread)
                          case retry of
                            GlobalWait i -> do
                                writeChan log ("rest - GLOBAL WAIT LIMIT: "
                                                    <> T.pack (show ((i - curtime) * 1000)))
                                threadDelay $ round ((i - curtime + 0.1) * 1000)
                                loop ratelocker
                            PathWait i -> loop $ M.insert route i (removeAllExpire ratelocker curtime)
                            NoLimit -> loop ratelocker

data RateLimited = Available | Locked

compareRate :: M.Map String POSIXTime -> String -> POSIXTime -> RateLimited
compareRate ratelocker route curtime =
    case M.lookup route ratelocker of
      Just unlockTime -> if curtime < unlockTime then Locked else Available
      Nothing -> Available

removeAllExpire :: M.Map String POSIXTime -> POSIXTime -> M.Map String POSIXTime
removeAllExpire ratelocker curtime =
  if M.size ratelocker > 100 then M.filter (> curtime) ratelocker
                             else ratelocker

data RequestResponse = ResponseTryAgain
                     | ResponseByteString BL.ByteString
                     | ResponseErrorCode Int B.ByteString B.ByteString
    deriving (Show)

data Timeout = GlobalWait POSIXTime
             | PathWait POSIXTime
             | NoLimit

tryRequest :: Chan T.Text -> RestIO R.LbsResponse -> RestIO (RequestResponse, Timeout)
tryRequest _log action = do
  resp <- action
  now <- liftIO getPOSIXTime
  let body   = R.responseBody resp
      code   = R.responseStatusCode resp
      status = R.responseStatusMessage resp
      global = (Just ("true" :: String) ==) $ readMaybeBS =<< R.responseHeader resp "X-RateLimit-Global"
      remain = fromMaybe 1 $ readMaybeBS =<< R.responseHeader resp "X-RateLimit-Remaining" :: Integer
      reset = withDelta . fromMaybe 10 $ readMaybeBS =<< R.responseHeader resp "X-RateLimit-Reset-After"

      withDelta :: Double -> POSIXTime
      withDelta dt = now + fromRational (toRational dt)

  if | code == 429 -> pure (ResponseTryAgain, if global then GlobalWait reset
                                                        else PathWait reset)
     | code `elem` [500,502] -> pure (ResponseTryAgain, NoLimit)
     | inRange (200,299) code -> pure ( ResponseByteString body
                                      , if remain > 0 then NoLimit else PathWait reset )
     | inRange (400,499) code -> pure (ResponseErrorCode code status (BL.toStrict body)
                                      , if remain > 0 then NoLimit else PathWait reset )
     | otherwise -> pure (ResponseErrorCode code status (BL.toStrict body), NoLimit)

readMaybeBS :: Read a => B.ByteString -> Maybe a
readMaybeBS = readMaybe . T.unpack . TE.decodeUtf8

compileRequest :: Auth -> JsonRequest -> RestIO R.LbsResponse
compileRequest auth request = action
  where
  authopt = authHeader auth <> R.header "X-RateLimit-Precision" "millisecond"

  action = case request of
    (Delete url      opts) -> R.req R.DELETE url R.NoReqBody R.lbsResponse (authopt <> opts)
    (Get    url      opts) -> R.req R.GET    url R.NoReqBody R.lbsResponse (authopt <> opts)
    (Put    url body opts) -> R.req R.PUT    url body        R.lbsResponse (authopt <> opts)
    (Patch  url body opts) -> do b <- body
                                 R.req R.PATCH  url b        R.lbsResponse (authopt <> opts)
    (Post   url body opts) -> do b <- body
                                 R.req R.POST   url b        R.lbsResponse (authopt <> opts)
