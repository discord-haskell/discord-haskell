{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import           Control.Monad (forever)
import           Control.Concurrent      -- Chans and Threads

import           Discord.Types
import qualified Discord.Requests as R

import           Discord.Internal.Rest (startRestThread, writeRestCall, RestCallInternalException(..))

import ExampleUtils (getToken, getGuildId)

{-
Peel back the `runDiscord` abstraction

Send an HTTP restcall without logging into the gateway
-}
main :: IO ()
main = do
  tok <- getToken
  testserverid <- getGuildId

  -- SETUP LOG
  printQueue <- newChan :: IO (Chan T.Text)
  printThreadId <- forkIO $ forever $ readChan printQueue >>= TIO.putStrLn

  -- START REST LOOP THREAD
  (restChan, restThreadId) <- startRestThread (Auth tok) printQueue

  -- a rest call to get the channels in which we will post a message
  Right cs <- writeRestCall restChan (R.GetGuildChannels testserverid)

  -- ONE REST CALL
  resp <- writeRestCall restChan (R.CreateMessage (channelId (head $ filter isTextChannel cs)) "Message")
  case resp of
    Right msg -> print $ "created message: " <> show msg
    Left (RestCallInternalErrorCode _code _status _body) -> print "4XX style http error code"
    Left (RestCallInternalHttpException _err) -> print "http exception (likely no connection)"
    Left (RestCallInternalNoParse _err _jsondata) -> print "can't parse return JSON"

  -- CLEANUP
  killThread printThreadId
  killThread restThreadId
  where
    isTextChannel :: Channel -> Bool
    isTextChannel ChannelText {} = True
    isTextChannel _ = False
