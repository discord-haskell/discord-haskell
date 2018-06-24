{-# LANGUAGE OverloadedStrings #-}

import Data.Monoid ((<>))
import Data.Char (isSpace)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Discord

a :: IO ()
a = do
  tok <- T.filter (not . isSpace) <$> TIO.readFile "./examples/auth-token.secret"
  Discord (RestPart rest) _ <- login (Bot tok)

  msg <- rest (CreateMessage 453207241294610444 "A" Nothing)
  putStrLn ("Message object: " <> show msg)

  putStrLn ""

  chan <- rest (GetChannel 453207241294610444)
  putStrLn ("Channel object: " <> show chan)

  return ()


