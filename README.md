# discord-haskell           [![Build Status](https://travis-ci.com/aquarial/discord-haskell.png?branch=master)](https://travis-ci.com/aquarial/discord-haskell)        [![Hackage version](http://img.shields.io/hackage/v/discord-haskell.svg?label=Hackage)](https://hackage.haskell.org/package/discord-haskell)

Build that discord bot in Haskell! Also checkout the [calamity](https://github.com/nitros12/calamity) 
haskell discord library for a more advanced interface than `discord-haskell`.

This is an example bot that replies "pong" to messages that start with "ping".

```haskell
{-# LANGUAGE OverloadedStrings #-}  -- allows "string literals" to be Text

import Control.Monad (when)
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
                                            , discordOnEvent = eventHandler }
                     TIO.putStrLn userFacingError

eventHandler :: Event -> DiscordHandler ()
eventHandler event = case event of
       MessageCreate m -> when (not (fromBot m) && isPing (messageText m)) $ do
               _ <- restCall (R.CreateReaction (messageChannel m, messageId m) "eyes")
               threadDelay (4 * 10^6)
               _ <- restCall (R.CreateMessage (messageChannel m) "Pong!")
               pure ()
       _ -> pure ()

fromBot :: Message -> Bool
fromBot m = userIsBot (messageAuthor m)

isPing :: Text -> Bool
isPing = ("ping" `isPrefixOf`) . toLower
```


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

For a list of rest requests, gateway events, and gateway sendables go to the 
[official discord documentation](https://discord.com/developers/docs/intro)

The rest requests line up very closely. The documentation lists 
[Get Channel](https://discord.com/developers/docs/resources/channel#get-channel)
and discord-haskell has `GetChannel :: ChannelId -> ChannelRequest Channel`. Same for gateway `Event`s.

#### Examples

The [examples](https://github.com/aquarial/discord-haskell/tree/master/examples) were crafted
to display a variety of use cases. Read them with care.

#### Open an Issue

For deeper questions about how the library functions, feel free to open an issue.

#### Discord server

Coming sometime!
