{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

import Control.Monad (forever, when, void)
import Data.Monoid ((<>))
import Data.Char (isSpace, toLower)
import qualified Data.ByteString.Char8 as Q
import qualified Data.Text as T

import Discord

a :: IO ()
a = do
  tok <- Q.filter (not . isSpace) <$> Q.readFile "./examples/auth-token.secret"

  RestPart rest <- loginRest (Bot tok)
  _ <- rest (CreateMessage 453207241294610444 "Hello!" Nothing)

  nextEvent <- loginGateway (Bot tok)
  forever $ do
      e <- nextEvent
      case e of
        MessageCreate m -> when (T.isPrefixOf "ping" (T.map toLower (messageContent m))) $ do
          void $ rest (CreateMessage 453207241294610444 "Pong!" Nothing)
        _ -> pure ()


