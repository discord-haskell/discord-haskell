### Installing

discord-haskell is on hosted on hackage at <https://hackage.haskell.org/package/discord-haskell>,

use the latest the HACKAGE_VERSION from the changelog
<https://github.com/discord-haskell/discord-haskell/blob/master/changelog.md>


#### Stack - Hackage (recommended)

In `stack.yaml`

```yaml
extra-deps:
- emoji-0.1.0.2
- discord-haskell-HACKAGE_VERSION
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






#### Cabal - Hackage

In `project.cabal`

```cabal
cabal-version:       2.0
name:                haskell-bot
version:             0.0.1
build-type:          Simple

executable haskell-bot
  main-is:             src/Main.hs
  default-language:    Haskell2010
  ghc-options:         -threaded
  build-depends:       base
                     , text
                     , discord-haskell == HACKAGE_VERSION
         --  check hackage for most recent available version:
         --   https://hackage.haskell.org/package/discord-haskell
```






#### Stack - GitHub

For specific/alternate versions not released on hackage. Only recommended if you're trying out a fork, or trying out a newer version.

## Stack - GitHub

In `stack.yaml`
```yaml
resolver: lts-16.20

extra-deps:
- git: git@github.com:discord-haskell/discord-haskell.git
  commit: SOME_COMMIT_HASH
   # go to https://github.com/discord-haskell/discord-haskell/commits/master
   # and click on a commit to view the commit hash
   # if you don't have a specific hash you're looking for,
   # I recommend using the `Stack - Hackage` style installing
- emoji-0.1.0.2
```

In `project.cabal`

```cabal
cabal-version:       2.0
name:                haskell-bot
version:             0.0.1
build-type:          Simple

executable haskell-bot
  main-is:             src/Main.hs
  default-language:    Haskell2010
  ghc-options:         -threaded
  build-depends:       base
                     , text
                     , discord-haskell
```
