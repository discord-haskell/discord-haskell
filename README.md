# discord-haskell [![Build Status](https://travis-ci.org/aquarial/discord-haskell.png?branch=master)](https://travis-ci.org/aquarial/discord-haskell)

## Go to the [Wiki](https://github.com/aquarial/discord-haskell/wiki) for more information

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
