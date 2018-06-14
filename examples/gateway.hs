{-# LANGUAGE OverloadedStrings #-}

import Control.Concurrent.Chan
import Control.Monad
import Data.Char (isSpace)
import Data.Monoid ((<>))
import qualified Data.ByteString.Char8 as Q

import Network.Discord.Gateway
import Network.Discord.Types

a :: IO ()
a = do
  tok <- Q.filter (not . isSpace) <$> Q.readFile "./examples/auth-token.secret"
  let da = DiscordAuth (Bot tok) "0.0.8"
  c <- newChan
  newSocket da c
  forever $ do
    x <- readChan c
    pure ()
    --putStrLn (show x <> "\n\n")

