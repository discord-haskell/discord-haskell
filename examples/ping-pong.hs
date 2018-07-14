{-# LANGUAGE OverloadedStrings #-}

import Control.Monad (forever, when)
import Data.Char (isSpace, toLower)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Discord

-- |Replies to every message that starts with "ping" with "pong"
pingpongExample :: IO ()
pingpongExample = do
  tok <- T.filter (not . isSpace) <$> TIO.readFile "./examples/auth-token.secret"

  dis <- loginRestGateway (Bot tok)

  forever $ do
      e <- nextEvent dis
      case e of
        MessageCreate m -> when (isPing (messageContent m)) $ do
          resp <- restCall dis (CreateMessage (messageChannel m) "Pong!" Nothing)
          putStrLn (show resp)
          putStrLn ""
        _ -> pure ()

isPing :: T.Text -> Bool
isPing = T.isPrefixOf "ping" . T.map toLower
