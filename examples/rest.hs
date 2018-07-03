{-# LANGUAGE OverloadedStrings #-}

import Data.Monoid ((<>))
import Data.Char (isSpace)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Discord

-- |Sends a message and then gets a channel, printing the results
a :: IO ()
a = do
  tok <- T.filter (not . isSpace) <$> TIO.readFile "./examples/auth-token.secret"
  dis <- login (Bot tok)

  msg <- rest dis (CreateMessage 453207241294610444 "A" Nothing)
  putStrLn ("Message object: " <> show msg)

  putStrLn ""

  chan <- rest dis (GetChannel 453207241294610444)
  putStrLn ("Channel object: " <> show chan)


