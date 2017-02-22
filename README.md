# Discord.hs
A Haskell wrapper for the Discord API

## Installing
Discord-hs isn't on hackage or stackage (Yet, I'll upload when documentation is in a good state.)  
In order to install:

```sh
git clone https://gitlab.com/jkoike/Discord.hs.git
cd discord.hs
stack install
```

## Using in a project

The preferred (and only supported) method of using discord.hs is through [stack](https://docs.haskellstack.org/en/stable/README/). Open your `stack.yaml`
and find the `packages` section. Replace it with the following:

```yaml
packages:
  - location: '.'
  - location:
      git: https://gitlab.com/jkoike/Discord.hs.git
      commit: master
    extra-dep: true
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
- Client doesn't close correctly

## Future goals:
- [Eta](https://github.com/typelead/eta) compatibility
- [HaLVM](https://github.com/GaloisInc/HaLVM) compatibility (maybe)
- Tighten properties/prove properties
- Command framework (expansion on above)
