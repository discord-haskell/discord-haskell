# Discord.hs
A Haskell wrapper for the Discord API

[![Hackage](https://img.shields.io/hackage/v/discord-hs.svg?style=flat-square)](http://hackage.haskell.org/package/discord-hs)
[![Hackage-Deps](https://img.shields.io/hackage-deps/v/discord-hs.svg?style=flat-square)](http://packdeps.haskellers.com/feed?needle=discord-hs)
[![Travis](https://img.shields.io/travis/jano017/Discord.hs.svg?style=flat-square)](https://travis-ci.org/jano017/Discord.hs)

## Using in a project

The preferred (and only supported) method of using discord.hs is through [stack](https://docs.haskellstack.org/en/stable/README/). Open your `stack.yaml`
and find the `extra-deps` section. Add the following:

```yaml
extra-deps:
  - discord-hs-0.2.1
```

Then open your project.cabal file and add `discord-hs` to your build-depends.

Alternatively, you can add `discord-hs` to your project.cabal file, and run
`stack solver --update-config`. This will let stack catch other missing dependencies
in your project and is most likely the better option.

## PingPong
```haskell
{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
import Data.Text
import Pipes

import Network.Discord

reply :: Message -> Text -> Effect DiscordM ()
reply Message{messageChannel=chan} cont = fetch' $ CreateMessage chan cont Nothing

main :: IO ()
main = runBot (Bot "TOKEN") $ do
  with ReadyEvent $ \(Init v u _ _ _) ->
    liftIO . putStrLn $ "Connected to gateway v" ++ show v ++ " as user " ++ show u

  with MessageCreateEvent $ \msg@Message{..} -> do
    when ("Ping" `isPrefixOf` messageContent && (not . userIsBot $ messageAuthor)) $
      reply msg "Pong!"
```

## Known issues:
- Init isn't parsing correctly
- Client doesn't close correctly
- Missing voice support

## Future goals:
- [Eta](https://github.com/typelead/eta) compatibility
- [HaLVM](https://github.com/GaloisInc/HaLVM) compatibility (maybe)
- Command framework (Posibly through compat layer with marvin?)
- Ditch wreq (not included in stack lts-8.2)
- Upload to stackage
