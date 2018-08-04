{-# LANGUAGE OverloadedStrings #-}

import Data.Monoid ((<>))
import Data.Char (isSpace)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Discord

-- | Get a channel, send a message, react to a message
restExample :: IO ()
restExample = do
  tok <- T.filter (not . isSpace) <$> TIO.readFile "./examples/auth-token.secret"
  dis <- loginRest (Auth tok)

  chan <- restCall dis (GetChannel 453207241294610444)
  putStrLn ("Channel object: " <> show chan <> "\n")

  msg <- restCall dis (CreateMessage 453207241294610444 "Creating a message" Nothing)
  putStrLn ("Message object: " <> show msg <> "\n")

  -- nextEvent would fail with a type error because rest.hs uses
  --       'loginRest' not 'loginRestGateway'
  --
  -- Couldn't match type ‘Discord.NotLoggedIntoGateway’ with ‘Gateway’
  --     Expected type: (RestChan, Gateway)
  --       Actual type: (RestChan, Discord.NotLoggedIntoGateway)
  --
  -- e <- nextEvent dis

  case msg of
    Right m -> do r <- restCall dis (CreateReaction (453207241294610444, messageId m)
                                                   ("🐮", Nothing))
                  putStrLn ("Reaction resp: " <> show r)
    _ -> putStrLn "Creating the message failed, couldn't react"



