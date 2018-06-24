{-# LANGUAGE OverloadedStrings #-}

import Control.Monad (forever, when, void)
import Data.Char (isSpace, toLower)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Discord

a :: IO ()
a = do
  tok <- T.filter (not . isSpace) <$> TIO.readFile "./examples/auth-token.secret"

  Discord (RestPart rest) nextEvent <- login (Bot tok)

  _ <- rest (CreateMessage 453207241294610444 "Hello!" Nothing)

  forever $ do
      e <- nextEvent
      case e of
        MessageCreate m -> when (isPing (messageContent m)) $ do
          void $ rest (CreateMessage 453207241294610444 "Pong!" Nothing)
        _ -> pure ()

isPing :: T.Text -> Bool
isPing = T.isPrefixOf "ping" . T.map toLower
