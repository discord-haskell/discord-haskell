{-# LANGUAGE OverloadedStrings #-}

import Data.Char (isSpace)

import Data.Time
import Data.Aeson
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as Q

import Network.Discord.Rest

a :: IO ()
a = do
  tok <- Q.filter (not . isSpace) <$> Q.readFile "./examples/auth-token.secret"
  handle <- createHandler (DiscordAuth (Bot tok) "0.1.0")

  print =<< restCall handle (CreateMessage 453207241294610444 "A" Nothing)
  putStrLn ""
  print =<< restCall handle (GetChannel 453207241294610444)

  return ()


