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
  print $ "Message object: " <> show msg


  chan <- restCall handle (GetChannel 453207241294610444)
  print $ "Channel object: " <> show chan

  return ()


