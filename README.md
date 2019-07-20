# discord-haskell [![Build Status](https://travis-ci.org/aquarial/discord-haskell.png?branch=master)](https://travis-ci.org/aquarial/discord-haskell)

## More information in the [Wiki](https://github.com/aquarial/discord-haskell/wiki)

```haskell
{-# LANGUAGE OverloadedStrings #-}  -- allows "string literals" to be Text

import Control.Monad (when, void)
import Data.Default (def)
import Data.Text (isPrefixOf, toLower, Text)

import Discord

-- | Replies "pong" to every message that starts with "ping"
pingpongExample :: IO ()
pingpongExample = runDiscord $ def { discordToken = "Bot ZZZZZZZZZZZZZZZZZZZ"
                                   , discordOnEvent = eventHandler }

eventHandler :: DiscordHandle -> Event -> IO ()
eventHandler dis event = case event of
      MessageCreate m -> when (not (fromBot m) && isPing (messageText m)) $ do
        void $ restCall dis (CreateMessage (messageChannel m) "Pong!")
      _ -> pure ()

fromBot :: Message -> Bool
fromBot m = userIsBot (messageAuthor m)

isPing :: Text -> Bool
isPing = ("ping" `isPrefixOf`) . toLower
```
