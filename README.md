# discord-haskell [![Build Status](https://travis-ci.org/aquarial/discord-haskell.png?branch=master)](https://travis-ci.org/aquarial/discord-haskell)


## I'm rewriting the interface for the library. Version [0.8.4](https://github.com/aquarial/discord-haskell/tree/0f906c0b7953027488a9915d93cb7eb7c0506b4c) is recommended for now.

## Information about the rewrite is in the [Wiki](https://github.com/aquarial/discord-haskell/wiki)

```haskell
{-# LANGUAGE OverloadedStrings #-}  -- allows "string literals" to be Text

import Control.Monad (when)
import Data.Text (isPrefixOf, toLower, Text)
import Control.Concurrent (threadDelay)
import qualified Data.Text.IO as TIO

import Discord
import Discord.Types
import qualified Discord.Requests as R

-- | Replies "pong" to every message that starts with "ping"
pingpongExample :: IO ()
pingpongExample = do userFacingError <- runDiscord $ def
                                            { discordToken = "Bot ZZZZZZZZZZZZZZZZZZZ"
                                            , discordOnEvent = eventHandler }
                     TIO.putStrLn userFacingError

eventHandler :: DiscordHandle -> Event -> IO ()
eventHandler dis event = case event of
       MessageCreate m -> when (not (fromBot m) && isPing (messageText m)) $ do
               _ <- restCall dis (R.CreateReaction (messageChannel m, messageId m) "eyes")
               threadDelay (4 * 10^6)
               _ <- restCall dis (R.CreateMessage (messageChannel m) "Pong!")
               pure ()
       _ -> pure ()

fromBot :: Message -> Bool
fromBot m = userIsBot (messageAuthor m)

isPing :: Text -> Bool
isPing = ("ping" `isPrefixOf`) . toLower
```
