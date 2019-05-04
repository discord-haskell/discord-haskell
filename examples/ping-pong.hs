{-# LANGUAGE OverloadedStrings #-}

import Control.Exception (finally)
import Control.Monad (when)
import Data.Char (toLower)
import Data.Monoid ((<>))
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Discord

-- | Replies "pong" to every message that starts with "ping"
pingpongExample :: IO ()
pingpongExample = do
  tok <- T.strip <$> TIO.readFile "./examples/auth-token.secret"

  dis <- loginRestGateway (Auth tok)

  finally (loopingPing dis)
          (stopDiscord dis)

loopingPing :: (RestChan, Gateway, z) -> IO ()
loopingPing dis = do
  e <- nextEvent dis
  case e of
      Left er -> putStrLn ("Event error: " <> show er)
      Right (MessageCreate m) -> do
        when (isPing (messageText m) && not (fromBot m)) $ do
          resp <- restCall dis (CreateMessage (messageChannel m) "Pong!")
          putStrLn (show resp)
          putStrLn ""
        loopingPing dis
      _ -> loopingPing dis

fromBot :: Message -> Bool
fromBot m = case messageAuthor m of
              Right u -> userIsBot u
              Left _webhookid -> True

isPing :: T.Text -> Bool
isPing = T.isPrefixOf "ping" . T.map toLower
