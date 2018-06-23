{-# LANGUAGE OverloadedStrings #-}

import Data.Monoid ((<>))
import Data.Char (isSpace)
import qualified Data.ByteString.Char8 as Q

import Discord

a :: IO ()
a = do
  tok <- Q.filter (not . isSpace) <$> Q.readFile "./examples/auth-token.secret"
  RestPart rest <- loginRest (Bot tok)

  msg <- rest (CreateMessage 453207241294610444 "A" Nothing)
  putStrLn ("Message object: " <> show msg)

  putStrLn ""

  chan <- rest (GetChannel 453207241294610444)
  putStrLn ("Channel object: " <> show chan)

  return ()


