{-# LANGUAGE OverloadedStrings #-}

import Control.Concurrent.Chan
import Control.Monad (forever)
import Data.Char (isSpace)
import Data.Monoid ((<>))
import qualified Data.ByteString.Char8 as Q

import Discord.Gateway
import Discord.Types

a :: IO ()
a = do
  tok <- Q.filter (not . isSpace) <$> Q.readFile "./examples/auth-token.secret"
  c <- chanWebSocket (Bot tok)
  forever $ do
    x <- readChan c
    putStrLn (show x <> "\n")
    pure ()

