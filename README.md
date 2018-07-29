# discord-haskell

## Quick notes

Use `Control.Exception.finally` with `stopDiscord` to safely
kill background threads.
(otherwise exit ghci and reopen to kill threads)

If you don't need to use `nextEvent` you should login with `loginRest`. 
`loginRestGateway` will fill up a `Chan Event` as time goes on.

If the library crashes look at `the-log-of-discord-haskell.txt` file.

The examples will work on the `master` branch. The `dev` branch
has the most recent (potentially) breaking changes.

## Getting Started

1 Create a bot to get a clientid and secret
<https://discordapp.com/developers/applications/me>

2 Figure out what permissions you need
<https://discordapi.com/permissions.html>

3 Invite the bot to a server
<https://discordapp.com/oauth2/authorize?client_id=CLIENT_ID&scope=bot&permissions=PERMISSIONS>

4 Connect to the gateway once in order to send CreateMessage events

5 Look at the examples.
[examples/gateway.hs](./examples/gateway.hs),
[examples/rest.hs](./examples/rest.hs),
[examples/cache.hs](./examples/cache.hs), and
[examples/ping-pong.hs](./examples/ping-pong.hs)

6 Add this library to your dependencies. discord-haskell
won't be uploaded to hackage for a while. In the meantime,
use github. discord-haskell is version
bound to match stackage lts-11.10

```
# in stack.yaml
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

- Double check the REST request ADT matches the API
- Add data types for
[activities](https://discordapp.com/developers/docs/topics/gateway#activity-object),
[permissions](https://discordapp.com/developers/docs/topics/permissions), and
[presences](https://discordapp.com/developers/docs/topics/gateway#presence-update)
- Update channel types (fill out guildcategory)
- Modify cache with Events
- Add gateway ToJSON for events
- Update comments to ADT types
- Upload to Hackage
