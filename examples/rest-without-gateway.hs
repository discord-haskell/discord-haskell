import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import           Control.Monad (forever)
import           Control.Concurrent      -- Chans and Threads

import           Discord.Types
import qualified Discord.Requests as R

import           Discord.Internal.Rest (startRestThread, writeRestCall, RestCallInternalException(..))

{-
Peel back the `runDiscord` abstraction

Send an HTTP restcall without logging into the gateway
-}
main :: IO ()
main = do
  tok <- TIO.readFile "./examples/auth-token.secret"

  let someChannelId = -1 -- TODO: CHANGE ME

  -- SETUP LOG
  printQueue <- newChan :: IO (Chan T.Text)
  printThreadId <- forkIO $ forever $ readChan printQueue >>= TIO.putStrLn

  -- START REST LOOP THREAD
  (restChan, restThreadId) <- startRestThread (Auth tok) printQueue

  -- ONE REST CALL
  resp <- writeRestCall restChan (R.CreateMessage someChannelId "Message")
  case resp of
    Right msg -> print "created"
    Left (RestCallInternalErrorCode code status body) -> print "4XX style http error code"
    Left (RestCallInternalHttpException err) -> print "http exception (likely no connection)"
    Left (RestCallInternalNoParse err jsondata) -> print "can't parse return JSON"

  -- CLEANUP
  killThread printThreadId
  killThread restThreadId
