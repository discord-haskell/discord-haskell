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

  finally (pingloop dis)
          (stopDiscord dis)

pingloop :: (RestChan, Gateway, z) -> IO ()
pingloop dis = do
  e <- nextEvent dis
  case e of
      Left er -> putStrLn ("Event error: " <> show er)
      Right (MessageCreate m) -> do
        when (isPing (messageText m)) $ do
          resp <- restCall dis (CreateMessage (messageChannel m) "Pong!")
          putStrLn (show resp)
          putStrLn ""
      _ -> pure ()
  pingloop dis

isPing :: T.Text -> Bool
isPing = T.isPrefixOf "ping" . T.map toLower
