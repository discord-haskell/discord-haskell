{-# LANGUAGE OverloadedStrings #-}

import Control.Monad (forever, when, void)
import Data.Char (isSpace, toLower)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Discord

-- |Replies to every message that starts with "ping" with "pong"
a :: IO ()
a = do
  tok <- T.filter (not . isSpace) <$> TIO.readFile "./examples/auth-token.secret"

  dis <- login (Bot tok)

  _ <- rest dis (CreateMessage 453207241294610444 "Hello!" Nothing)

  forever $ do
      e <- nextEvent dis
      case e of
        MessageCreate m -> when (isPing (messageContent m)) $ do
          void $ rest dis (CreateMessage 453207241294610444 "Pong!" Nothing)
        _ -> pure ()

isPing :: T.Text -> Bool
isPing = T.isPrefixOf "ping" . T.map toLower
