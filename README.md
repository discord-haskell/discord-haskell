# discord-haskell           [![CI Status](https://github.com/aquarial/discord-haskell/actions/workflows/main.yml/badge.svg)](https://github.com/aquarial/discord-haskell/actions/)        [![Hackage version](http://img.shields.io/hackage/v/discord-haskell.svg?label=Hackage)](https://hackage.haskell.org/package/discord-haskell)              [![Discord server](https://discord.com/api/guilds/918577626954739722/widget.png?style=shield)](https://discord.gg/eaRAGgX3bK)

## About

Build that discord bot in Haskell! Also checkout the [calamity haskell library](https://github.com/nitros12/calamity)
for a more advanced interface than `discord-haskell`.

### Discord Server

Created a discord server: <https://discord.gg/eaRAGgX3bK>

Ask questions, get updates, request features, etc

## Implementation and Progress

### Quickstart

This is an example bot that replies "pong" to messages that start with "ping". Also checkout the [other examples](./examples/)

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
pingpongExample = do userFacingError <- runDiscord $ def
                                            { discordToken = "Bot ZZZZZZZZZZZZZZZZZZZ"
                                            , discordOnEvent = eventHandler
                                            , discordOnLog = \s -> TIO.putStrLn s }
                     TIO.putStrLn userFacingError
                     -- only reached on an unrecoverable error
                     -- put normal 'cleanup' code in discordOnEnd

eventHandler :: Event -> DiscordHandler ()
eventHandler event = case event of
       -- eventHandler is called concurrently unless discordForkThreadForEvents = False
       MessageCreate m -> when (not (fromBot m) && isPing m) $ do
               void $ restCall (R.CreateReaction (messageChannel m, messageId m) "eyes")
               threadDelay (2 * 10^6)
               void $ restCall (R.CreateMessage (messageChannel m) "Pong!")
       _ -> return ()

fromBot :: Message -> Bool
fromBot = userIsBot . messageAuthor

isPing :: Message -> Bool
isPing = ("ping" `isPrefixOf`) . toLower . messageText
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

discord-haskell is on hosted on hackage at <https://hackage.haskell.org/package/discord-haskell>,

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

### Emoji

For single character Emoji you can use the unicode name ("eyes", "fire", etc).

For multi-character Emoji you must use the discord format. Type `\:emoji:` into
a discord chat and paste that into the Text

For example `:thumbsup::skin-tone-3:` is `"👍\127997"`.
A custom emoji will look like `<name:id_number>` or `name:id_number`.

See [examples/ping-pong.hs](https://github.com/aquarial/discord-haskell/blob/master/examples/ping-pong.hs)
 for a `CreateReaction` request in use.

### Embeds

Embeds are special messages with boarders and images. [Example embed created by discord-haskell](./examples/embed-photo.jpg)

The `Embed` record (and sub-records) store embed data received from Discord.

The `CreateEmbed` record stores data when we want to create an embed.

`CreateEmbed` has a `Default` instance, so you only need to specify the fields you use:

```haskell
_ <- restCall (R.CreateMessageEmbed <channel_id> "Pong!" $
        def { createEmbedTitle = "Pong Embed"
            , createEmbedImage = Just $ CreateEmbedImageUpload <bytestring>
            , createEmbedThumbnail = Just $ CreateEmbedImageUrl
                    "https://avatars2.githubusercontent.com/u/37496339"
            })
```

Uploading a file each time is slow, prefer uploading images to a hosting site like imgur.com, and then referencing them.

### Limitations

The following features are not implemented:

- Voice & Audio
- Authenticating with a user token

### Debugging

Always print the userFacingError Text returned from `runDiscord`. I use this to record
errors that cannot be recovered from.

If something else goes wrong with the library please open an issue. It is helpful,
but not always necessary, to attach a log of what's going on when the library
crashes.

Assign a handler to the `discordOnLog :: Text -> IO ()` to print info as it happens.
Remember to remove sensitive information before posting.

### Getting Help

#### Official discord docs

For a list of rest requests, gateway events, and gateway sendables go ggto the
[official discord documentation](https://discord.com/developers/docs/intro)

The rest requests line up very closely. The documentation lists
[Get Channel](https://discord.com/developers/docs/resources/channel#get-channel)
and discord-haskell has `GetChannel :: ChannelId -> ChannelRequest Channel`. Same for gateway `Event`s.

#### Examples

The [examples](https://github.com/aquarial/discord-haskell/tree/master/examples) were crafted
to display a variety of use cases. Read them with care.

#### Open an Issue

For deeper questions about how the library functions, feel free to open an issue.

#### Contributing

Open to code formatting. We're planning to use ormolu to autoformat the sourcecode eventually.
See [the formatting tracking issue](https://github.com/aquarial/discord-haskell/issues/87)
