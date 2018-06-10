{-# LANGUAGE OverloadedStrings #-}

-- | Provide HTTP primitives
module Network.Discord.Rest.HTTP
  ( restLoop
  , Resp(..)
  ) where

import Data.Semigroup ((<>))

import Control.Concurrent.MVar
import Control.Concurrent (threadDelay)
import Control.Concurrent.Chan
import Control.Exception (throwIO)
import Data.Aeson
import Data.Ix (inRange)
import Data.Time
import qualified Data.ByteString.Char8 as Q
import qualified Data.ByteString.Lazy.Char8 as QL
import Data.Default
import Data.Maybe (fromMaybe)
import Text.Read (readMaybe)
import qualified Network.HTTP.Req as R
import qualified Data.Map.Strict as M

import Network.Discord.Rest.Prelude
import Network.Discord.Types

data Resp a = Resp a
            | NoResp
            | BadResp String
  deriving (Show)

restLoop :: (FromJSON a, DiscordRequest req) =>
        DiscordAuth -> Chan (req a, MVar (Resp a)) -> IO ()
restLoop auth urls = loop M.empty
  where
  loop ratelocker = do
    (discReq, thread) <- readChan urls
    curtime <- getCurrentTime
    case compareRate ratelocker curtime (majorRoute discReq) of
      Locked -> do writeChan urls (discReq, thread)
                   loop ratelocker
      Available -> do let action = compileRequest auth (createRequest discReq)
                      (resp, timeout) <- tryRequest action
                      case decode <$> resp  of
                        Resp (Just r) -> putMVar thread (Resp r)
                        Resp Nothing  -> putMVar thread NoResp
                        NoResp        -> putMVar thread NoResp
                        BadResp "Try Again" -> writeChan urls (discReq, thread)
                        BadResp r -> putMVar thread (BadResp r)
                      case timeout of
                        GlobalWait i -> threadDelay (i * 1000) >> loop ratelocker
                        PathWait i -> loop $ M.insert (majorRoute discReq)
                                                      (addSeconds i curtime)
                                                      ratelocker
                        NoLimit -> loop ratelocker

addSeconds :: Int -> UTCTime -> UTCTime
addSeconds s = addUTCTime (fromIntegral s)

compareRate :: (Ord a, Ord k) => M.Map k a -> a -> k -> RateLimited
compareRate ratelocker curtime route =
    case M.lookup route ratelocker of
      Just unlockTime -> if curtime < unlockTime then Locked else Available
      Nothing -> Available

data RateLimited = Available | Locked

data Timeout = GlobalWait Int
             | PathWait Int
             | NoLimit

instance Functor Resp where
  fmap f (Resp a) = Resp (f a)
  fmap _ NoResp   = NoResp
  fmap _ (BadResp e)  = BadResp e

tryRequest :: IO R.LbsResponse -> IO (Resp QL.ByteString, Timeout)
tryRequest action = do
  resp <- action
  let code   = R.responseStatusCode resp
      status = R.responseStatusMessage resp
      wait   = fromMaybe  2000 $ readMaybeBS =<< R.responseHeader resp "Retry-After"
      global = fromMaybe False $ readMaybeBS =<< R.responseHeader resp "X-RateLimit-Global"
      remain = fromMaybe     1 $ readMaybeBS =<< R.responseHeader resp "X-Ratelimit-Remaining"
  case () of
   _ | code == 429 -> pure (BadResp "Try Again", if global then GlobalWait wait else PathWait wait)
     | code `elem` [500,502] -> pure (BadResp "Try Again", NoLimit)
     | inRange (200,299) code -> pure ( Resp (R.responseBody resp)
                                      , if remain > 0 then NoLimit else PathWait wait )
     | inRange (400,499) code -> pure ( BadResp (show code <> " - " <> Q.unpack status
                                                           <> QL.unpack (R.responseBody resp))
                                      , if remain > 0 then NoLimit else PathWait wait )
     | otherwise -> let err = "Unexpected code: " ++ show code ++ " - " ++ Q.unpack status
                    in pure (BadResp err, NoLimit)

readMaybeBS :: Read a => Q.ByteString -> Maybe a
readMaybeBS = readMaybe . Q.unpack

compileRequest :: (FromJSON r) => DiscordAuth -> JsonRequest r -> IO R.LbsResponse
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

instance R.MonadHttp IO where
  -- :: R.MonadHttp m => R.HttpException -> m a
  handleHttpException = throwIO
  getHttpConfig = pure $ def { R.httpConfigCheckResponse = \_ _ _ -> Nothing }

