{-# LANGUAGE OverloadedStrings #-}

import Data.Monoid ((<>))
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Discord

-- | Get a channel, send a message, react to a message
restExample :: IO ()
restExample = do
  tok <- T.strip <$> TIO.readFile "./examples/auth-token.secret"
  dis <- loginRest (Auth tok)
  let chanid = 517170652092825610
      -- guildid = 453207241294610444

  chan <- restCall dis (GetChannel chanid)
  putStrLn ("Channel object: " <> show chan <> "\n")

  msg <- restCall dis (CreateMessage chanid "Creating a message")
  putStrLn ("Message object: " <> show msg <> "\n")

  case msg of
    Right m -> do r0 <- restCall dis (CreateReaction (chanid, messageId m) "🐮")
                  putStrLn ("Reaction resp: " <> show r0)
                  r1 <- restCall dis (CreateReaction (chanid, messageId m)
                                             "<customemoji:519649707388174337>")
                                             -- type \:emoji: in discord
                  putStrLn ("Reaction resp: " <> show r1)
    _ -> putStrLn "Creating the message failed, couldn't react"

  stopDiscord dis

  -- nextEvent would fail with a type error because rest.hs uses
  --       'loginRest' not 'loginRestGateway'
  --
  -- Couldn't match type ‘Discord.NotLoggedIntoGateway’ with ‘Gateway’
  --     Expected type: (RestChan, Gateway)
  --       Actual type: (RestChan, Discord.NotLoggedIntoGateway)
  --
  -- e <- nextEvent dis



