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
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Text.Read (readMaybe)
import Data.Maybe (fromMaybe)
import qualified Data.Map.Strict as M

import Discord.Internal.Types
import Discord.Internal.Rest.Prelude
import qualified Network.HTTP.Client as N (Request) -- disambiguate with our Request type
import Network.HTTP.Client (method, requestHeaders, requestBody, HttpException, parseRequest, httpLbs, responseBody, responseStatus, responseHeaders)
import Network.HTTP.Client.TLS (getGlobalManager)
import Network.HTTP.Types (Status(..))
import Network.HTTP.Client.MultipartFormData (formDataBody)

-- | An exception in a Rest call
data RestCallInternalException
  -- | Error code from Discord
  = RestCallInternalErrorCode Int B.ByteString B.ByteString
  -- | Couldn't parse the response
  | RestCallInternalNoParse String BL.ByteString
  -- | Something went bad in the HTTP process
  | RestCallInternalHttpException HttpException
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
      Available -> do action <- compileRequest auth request
                      reqIO <- try $ (tryRequest log action)
                      case reqIO :: Either HttpException (RequestResponse, Timeout) of
                        Left e -> do
                          writeChan log ("rest - http exception " <> T.pack (show e))
                          putMVar thread (Left (RestCallInternalHttpException e))
                          loop ratelocker
                        Right (resp, retry) -> do
                          case resp of
                            -- decode "[]" == () for expected empty calls
                            ResponseByteString "" -> putMVar thread (Right "[]")
                            ResponseByteString bs -> putMVar thread (Right bs)
                            ResponseErrorCode e s b -> do
                              print $ "failed request: " <> show action
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

tryRequest :: Chan T.Text -> N.Request -> IO (RequestResponse, Timeout)
tryRequest _log action = do
  resp <- getGlobalManager >>= \manager -> httpLbs action manager
  now <- liftIO getPOSIXTime
  let body   = responseBody resp
      code   = statusCode $ responseStatus resp
      status = statusMessage $ responseStatus resp
      headers = responseHeaders resp
      getHeader h = readMaybeBS =<< (lookup h headers)
      global = (Just ("true" :: String) ==) $ getHeader "X-RateLimit-Global"
      remain = fromMaybe 1 $ getHeader "X-RateLimit-Remaining" :: Integer
      reset = withDelta . fromMaybe 10 $ getHeader "X-RateLimit-Reset-After"

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

compileRequest :: Auth -> JsonRequest -> IO N.Request
compileRequest auth request = action
  where
  authopt = authHeader auth
  defaultopt = [("X-RateLimit-Precision", BS8.pack "millisecond"), ("Content-Type", BS8.pack "application/json")]
  dedupHeaders hs = M.toList $ M.fromListWith (\new _ -> new) hs

  action = case request of
    (Delete url      opts) -> do
      req <- parseRequest $ T.unpack url
      pure req {
        method = "DELETE",
        requestHeaders = dedupHeaders $ defaultopt <> authopt <> opts
      }
    (Get    url      opts) -> do
      req <- parseRequest $ T.unpack url
      pure req {
        method = "GET",
        requestHeaders = dedupHeaders $ defaultopt <> authopt <> opts
      }
    (Put    url body opts) -> do
      req <- parseRequest $ T.unpack url
      case body of
        Left b -> pure req {
          method = "PUT",
          requestBody = b,
          requestHeaders = dedupHeaders $ defaultopt <> authopt <> opts
        }
        Right parts -> do
          req' <- formDataBody parts req
          pure req' {
            method = "PUT",
            requestHeaders = dedupHeaders $ defaultopt <> authopt <> requestHeaders req' <> opts
          }
    (Patch  url body opts) -> do
      req <- parseRequest $ T.unpack url
      case body of
        Left b -> pure req {
          method = "PATCH",
          requestBody = b,
          requestHeaders = dedupHeaders $ defaultopt <> authopt <> opts
        }
        Right parts -> do
          req' <- formDataBody parts req
          pure req' {
            method = "PATCH",
            requestHeaders = dedupHeaders $ defaultopt <> authopt <> requestHeaders req' <> opts
          }
    (Post   url body opts) -> do
      req <- parseRequest $ T.unpack url
      case body of
        Left b -> pure req {
          method = "POST",
          requestBody = b,
          requestHeaders = dedupHeaders $ defaultopt <> authopt <> opts
        }
        Right parts -> do
          req' <- formDataBody parts req
          pure req' {
            method = "POST",
            requestHeaders = dedupHeaders $ defaultopt <> authopt <> requestHeaders req' <> opts
          }
