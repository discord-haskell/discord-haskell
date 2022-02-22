# discord-haskell        [![CI Status](https://github.com/aquarial/discord-haskell/actions/workflows/main.yml/badge.svg)](https://github.com/aquarial/discord-haskell/actions/)        [![Hackage version](http://img.shields.io/hackage/v/discord-haskell.svg?label=Hackage)](https://hackage.haskell.org/package/discord-haskell)              [![Discord server](https://discord.com/api/guilds/918577626954739722/widget.png?style=shield)](https://discord.gg/eaRAGgX3bK)


Build that discord bot in Haskell! Also checkout the
[calamity haskell library](https://github.com/nitros12/calamity)
for a more advanced interface.



#### Local Documentation

Documentation on specific features and metadata is in [the local documentation folder](./docs/README.md#Documentation)

#### Discord Server

Ask questions, get updates, request features, etc in the project discord server: <https://discord.gg/eaRAGgX3bK>

#### Official Discord Documentation

This api closley matches the [official discord documentation](https://discord.com/developers/docs/intro),
which lists the rest requests, gateway events, and gateway sendables.

You can use the docs to check the name of something you want to do. For example: 
the docs list a [Get Channel](https://discord.com/developers/docs/resources/channel#get-channel) API path,
which translates to discord-haskell's rest request ADT for `GetChannel` of type `ChannelId -> ChannelRequest Channel`. 

#### Open an Issue

If something goes wrong: check the error message (optional: check [the debugging logs](./docs/debugging.md)), make sure you have the most recent version, ask on discord, or open a github issue.

### Quickstart

This is an example bot that replies "pong" to messages that start with "ping". Also checkout the [other examples](./examples/) for things like state management.

```haskell
{-# LANGUAGE OverloadedStrings #-}  -- allows "string literals" to be Text
import           Control.Monad (when, void)
import           UnliftIO.Concurrent
import           Data.Text (isPrefixOf, toLower, Text)
import qualified Data.Text.IO as TIO

import           Discord
import           Discord.Types
import qualified Discord.Requests as R

-- | Replies "pong" to every message that starts with "ping"
pingpongExample :: IO ()
pingpongExample = do 
    userFacingError <- runDiscord $ def
             { discordToken = "Bot ZZZZZZZZZZZZZZZZZZZ"
             , discordOnEvent = eventHandler    }
    TIO.putStrLn userFacingError
    -- userFacingError is an unrecoverable error
    -- put normal 'cleanup' code in discordOnEnd (see examples)

eventHandler :: Event -> DiscordHandler ()
eventHandler event = case event of
       MessageCreate m -> when (isPing m && (not (fromBot m)) $ do
               void $ restCall (R.CreateReaction (messageChannelId m, messageId m) "eyes")
               threadDelay (2 * 10^6)
               void $ restCall (R.CreateMessage (messageChannelId m) "Pong!")
       _ -> return ()

fromBot :: Message -> Bool
fromBot = userIsBot . messageAuthor

isPing :: Message -> Bool
isPing = ("ping" `isPrefixOf`) . toLower . messageContent
```

### Installing

discord-haskell is on hosted on hackage at https://hackage.haskell.org/package/discord-haskell, 

Add the following to your `stack.yaml` (if using stack)

```yaml
extra-deps:
- emoji-0.1.0.2
- discord-haskell-VERSION
```

And add this to the `project.cabal`

```cabal
executable haskell-bot
  main-is:             src/Main.hs
  default-language:    Haskell2010
  ghc-options:         -threaded
  build-depends:       base
                     , text
                     , discord-haskell 
             --                          == VERSION      (if using cabal)
```

For a more complete example with various options go to 
[Installing the Library](https://github.com/aquarial/discord-haskell/wiki/Installing-the-Library) wiki page

Also take a look at 
[Creating your first Bot](https://github.com/aquarial/discord-haskell/wiki/Creating-your-first-Bot)
for some help setting up your bot token



### Rough Roadmap

- [ ] Add slash commands [issue](https://github.com/aquarial/discord-haskell/issues/59)
- [ ] APIv9 [issue](https://github.com/aquarial/discord-haskell/issues/72)
- [ ] Event type includes roles and other cached info
- [ ] higher level bot interface? easier to add state and stuff

 
