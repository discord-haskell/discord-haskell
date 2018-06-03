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
  , (R./:)
  ) where

import Data.Semigroup ((<>))

import Control.Concurrent.Chan
import Control.Concurrent.MVar
import Control.Concurrent (threadDelay)
import Control.Monad (when)
import Control.Monad.IO.Class
import Control.Concurrent.Chan
import Data.Aeson
import qualified Data.ByteString.Char8 as Q
import Data.Hashable
import Data.Maybe (fromMaybe)
--import qualified Data.Text as T
import qualified Network.HTTP.Req as R
import qualified Data.Map.Strict as M

import Network.Discord.Rest.Channel
import Network.Discord.Rest.Prelude
import Network.Discord.Types

restHandler :: (FromJSON a, DiscordRequest req) =>
        DiscordAuth -> Chan (req, MVar (Either String a)) -> IO ()
restHandler auth urls = loop M.empty
  where
  loop ratelocker = do
    (request, thread) <- readChan urls
    curtime <- round <$> getPOSIXTime
    case compareRate ratelocker curtime (majorRoute request) of
      Locked -> do writeChan urls (request, thread)
                   loop ratelocker
      Available -> do action <- compileJsonRequest request
                      (resp, timeout) <- trytillsuccess action
                      case resp of
                        Resp r -> putMVar thread (Right r)
                        BadResp r -> putMVar threaad (Left r)
                        TryAgain -> writeChan urls (request, thread)
                      case timeout of
                        GlobalWait i -> threadDelay (i * 1000) >> loop ratelocker
                        PathWait i -> loop $ M.insert (majorRoute request)
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

trytillsuccess :: DiscordRequest r => r -> IO (Resp a, Timeout)
trytillsuccess action = do
  resp <- action
  let code   = R.responseStatusCode resp
      status = R.responseStatusMessage resp
      wait   = fromMaybe  2000 $ R.responseHeader resp "Retry-After"
      global = fromMaybe False $ R.responseHeader resp "X-RateLimit-Global"
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

fetch :: (FromJSON r) => DiscordAuth -> JsonRequest r -> IO (R.JsonResponse r)
fetch auth request = case request of
    (Delete url      opts) -> R.req R.DELETE url R.NoReqBody R.jsonResponse (authopt <> opts)
    (Get    url      opts) -> R.req R.GET    url R.NoReqBody R.jsonResponse (authopt <> opts)
    (Patch  url body opts) -> R.req R.PATCH  url body        R.jsonResponse (authopt <> opts)
    (Post   url body opts) -> R.req R.POST   url body        R.jsonResponse (authopt <> opts)
    (Put    url body opts) -> R.req R.PUT    url body        R.jsonResponse (authopt <> opts)
  where
  authopt = authHeader auth


--makeRequest :: (FromJSON r, DiscordRest m, DoFetch f r)
--  => f r -> JsonRequest r -> m r
--makeRequest req action = do
--  waitRateLimit req
--  resp <- fetch action
--  when (parseHeader resp "X-RateLimit-Remaining" 1 < 1) $
--    setRateLimit req $ parseHeader resp "X-RateLimit-Reset" 0
--  return $ R.responseBody resp
--  where
--    parseHeader :: R.HttpResponse resp => resp -> ByteString -> Int -> Int
--    parseHeader resp header def = fromMaybe def $ decodeStrict =<< R.responseHeader resp header

