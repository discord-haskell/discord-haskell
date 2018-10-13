# discord-haskell

Please read `Notes` and `Getting Started` carefully

## Notes

`loginRest` allows `restCall`. `loginRestGateway` allows `restCall`,
`nextEvent`, and `readCache`. **Use `loginRest` if you don't need the 
gateway**

If the library crashes look at `the-log-of-discord-haskell.txt` file.

Use `Control.Exception.finally` with `stopDiscord` to safely
kill background threads when running examples in ghci
(otherwise exit ghci and reopen to kill threads)

The examples will work on the `master` branch. The `dev` branch
has the most recent (potentially) breaking changes.

## Getting Started

1 Create a bot to get a token, clientid, and secret
<https://discordapp.com/developers/applications/me> (connect with token)

2 Figure out what permissions you need
<https://discordapi.com/permissions.html>

3 Invite the bot to a server
`https://discordapp.com/oauth2/authorize?client_id= <CLIENT_ID> &scope=bot&permissions= <PERMISSIONS>`

4 Connect to the gateway once in order to send CreateMessage events.
[this is a discord requirement](https://discordapp.com/developers/docs/resources/channel#create-message)

5 Look at the examples.
[examples/gateway.hs](./examples/gateway.hs),
[examples/rest.hs](./examples/rest.hs),
[examples/cache.hs](./examples/cache.hs), and
[examples/ping-pong.hs](./examples/ping-pong.hs)

6 Skim documentation on 
[hackage](https://hackage.haskell.org/package/discord-haskell-0.5.2)

7 Add this library to your dependencies. discord-haskell is on hackage
with strict version bounds to stackage lts-11.10. You can also use
the github repo.

```
# in stack.yaml (if using stack)
resolver: lts-11.10
extra-deps:
- git: git@github.com:aquarial/discord-haskell.git
  commit: <most recent master commit>
  extra-dep: true

# in project.cabal
  build-depends:       base
                     , discord-haskell

```

## History

This library was originally forked from
[discord.hs](https://github.com/jano017/Discord.hs).
After rewriting the gateway/rest loops and extending the types
I think it makes more sense to present this library as
separate form the source. The apis are not compatible.

## TO DO

In roughly the order I'm working on them:

- Finish REST request ADT. Search for `-- todo` pattern
- Add data types for
[activities](https://discordapp.com/developers/docs/topics/gateway#activity-object),
[permissions](https://discordapp.com/developers/docs/topics/permissions), and
[presences](https://discordapp.com/developers/docs/topics/gateway#presence-update)
- Update channel types (fill out guildcategory)
- Modify cache with Events
- Add gateway ToJSON for events
- Update comments on ADT types

