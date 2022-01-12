# discord-haskell           [![CI Status](https://github.com/aquarial/discord-haskell/actions/workflows/main.yml/badge.svg)](https://github.com/aquarial/discord-haskell/actions/)        [![Hackage version](http://img.shields.io/hackage/v/discord-haskell.svg?label=Hackage)](https://hackage.haskell.org/package/discord-haskell)              [![Discord server](https://discord.com/api/guilds/918577626954739722/widget.png?style=shield)](https://discord.gg/eaRAGgX3bK)

## About

Build that discord bot in Haskell! Also checkout the [calamity haskell library](https://github.com/nitros12/calamity)
for a more advanced interface than `discord-haskell`.

### Documentation 

#### Local Documentation

Find docs on features, metadata, and parts is found in [./docs/README.md#Documentation](./docs/README.md#Documentation)

#### Discord Server

Current project discord server: <https://discord.gg/eaRAGgX3bK>

Ask questions, get updates, request features, etc

#### Official Discord Documentation

The [official discord documentation](https://discord.com/developers/docs/intro)
lists the rest requests, gateway events, and gateway sendables. The library
matches very closely. 

For example: 
[Get Channel](https://discord.com/developers/docs/resources/channel#get-channel)
and discord-haskell has a rest request ADT including 
`[GetChannel :: ChannelId -> ChannelRequest Channel]`. Similar matching exists for gateway `Event`s.

#### Open an Issue

If something goes wrong: check the error message, (optional: check log file), make sure you have the most recent version, ask on discord, or open a github issue.

## Implementation and Progress

### Quickstart

This is an example bot that replies "pong" to messages that start with "ping". Also checkout the [other examples](./examples/) for things like state management.

```haskell
{-# LANGUAGE OverloadedStrings #-}  -- allows "string literals" to be Text
import Control.Monad (when, void)
import Data.Text (isPrefixOf, toLower, Text)
import qualified Data.Text.IO as TIO
import UnliftIO.Concurrent
import Discord
import Discord.Types
import qualified Discord.Requests as R

-- | Replies "pong" to every message that starts with "ping"
pingpongExample :: IO ()
pingpongExample = do 
    userFacingError <- runDiscord $ def
             { discordToken = "Bot ZZZZZZZZZZZZZZZZZZZ"
             , discordOnEvent = eventHandler
             , discordOnLog = \s -> TIO.putStrLn s
             , discordForkThreadForEvents = True }
    TIO.putStrLn userFacingError
    -- userFacingError is an unrecoverable error
    -- put normal 'cleanup' code in discordOnEnd (see examples)

eventHandler :: Event -> DiscordHandler ()
eventHandler event = case event of
       MessageCreate m -> when (not (fromBot m) && isPing m) $ do
               void $ restCall (R.CreateReaction (messageChannelId m, messageId m) "eyes")
               threadDelay (2 * 10^6)
               void $ restCall (R.CreateMessage (messageChannelId m) "Pong!")
       _ -> return ()

fromBot :: Message -> Bool
fromBot = userIsBot . messageAuthor

isPing :: Message -> Bool
isPing = ("ping" `isPrefixOf`) . toLower . messageContent
```

```cabal
-- ping-pong.cabal

executable haskell-bot
  main-is:             src/Main.hs
  default-language:    Haskell2010
  ghc-options:         -threaded
  build-depends:       base
                     , text
                     , unliftio
                     , discord-haskell
```

### Biggest TODOs

- [ ] Add slash commands [issue](https://github.com/aquarial/discord-haskell/issues/59)
- [ ] APIv9 [issue](https://github.com/aquarial/discord-haskell/issues/72)
- [ ] Event type includes roles and other cached info
- [ ] higher level bot interface? easier to add state and stuff
- [x] rewrite eventloop [issue](https://github.com/aquarial/discord-haskell/issues/70)

### Installing

discord-haskell is on hosted on hackage at https://hackage.haskell.org/package/discord-haskell, 

In `stack.yaml`

```yaml
extra-deps:
- emoji-0.1.0.2
- discord-haskell-VERSION
```

In `project.cabal`

```cabal
executable haskell-bot
  main-is:             src/Main.hs
  default-language:    Haskell2010
  ghc-options:         -threaded
  build-depends:       base
                     , text
                     , discord-haskell
```

For a more complete example with various options go to 
[Installing the Library](https://github.com/aquarial/discord-haskell/wiki/Installing-the-Library) wiki page

Also take a look at 
[Creating your first Bot](https://github.com/aquarial/discord-haskell/wiki/Creating-your-first-Bot)
for some help setting up your bot token


 
