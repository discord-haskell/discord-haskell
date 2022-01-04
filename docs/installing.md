
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
