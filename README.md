# Discord.hs
A Haskell wrapper for the Discord API

[![Hackage](https://img.shields.io/hackage/v/discord-hs.svg)](http://hackage.haskell.org/package/discord-hs)
[![Hackage-Deps](https://img.shields.io/hackage-deps/v/discord-hs.svg?style=flat-square)](http://packdeps.haskellers.com/feed?needle=discord-hs)
[![Travis](https://img.shields.io/travis/jano017/Discord.hs.svg)](https://travis-ci.org/jano017/Discord.hs)

## Using in a project

The preferred (and only supported) method of using discord.hs is through [stack](https://docs.haskellstack.org/en/stable/README/). Open your `stack.yaml`
and find the `packages` section. Replace it with the following:

```yaml
packages:
  - '.'
  - discord-hs-0.1.3
```

Then open your project.cabal file and add `discord-hs` to your build-depends.


## PingPong
```haskell
{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
import Data.Text
import Pipes

import Network.Discord
import Language.Discord

reply :: Message -> Text -> Effect DiscordM ()
reply Message{messageChannel=chan} cont = fetch' $ CreateMessage chan cont

main :: IO ()
main = runBot "TOKEN" $ do
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
