{-# LANGUAGE OverloadedStrings #-}

import Data.Monoid ((<>))
import Data.Char (isSpace)

import qualified Data.ByteString.Char8 as Q

import Discord.Rest

a :: IO ()
a = do
  tok <- Q.filter (not . isSpace) <$> Q.readFile "./examples/auth-token.secret"
  handle <- createHandler (Bot tok)

  msg <- restCall handle (CreateMessage 453207241294610444 "A" Nothing)
  putStrLn $ "Message object: " <> show msg

  putStrLn ""

  chan <- restCall handle (GetChannel 453207241294610444)
  putStrLn $ "Channel object: " <> show chan

  return ()


