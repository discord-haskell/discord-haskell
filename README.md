# discord-haskell

## Quick notes

rest/gateway background threads aren't killed on ^C in ghci 
(exit ghci and reopen to kill threads)

If you don't need to use `nextEvent` you should login with `loginRest`. 
`loginRestGateway` uses a `Chan` that will fill up as time goes on.

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

6 Add this library to your dependencies. The library
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

I started discord-haskell by forking
[discord.hs](https://github.com/jano017/Discord.hs), but
I have since rewritten all of the rest and gateway logic.

## TO DO

In roughly the order I'm working on them:

- Add means of killing gateway/rest threads
- Double check the REST request ADT matches the API
- Add      [Activity object](https://discordapp.com/developers/docs/topics/gateway#activity-object) for PresenceInfo in the Event ADT
- Add neat [permission adt](https://discordapp.com/developers/docs/topics/permissions) handling
- Add neat [presence adt](https://discordapp.com/developers/docs/topics/gateway#presence-update) handling
- Add all gateway types (emoji removed, etc)
- Update channel types (fill out guildcategory)
- Update cache with Events
- More helpful README (examples, how to depend it this package)
- Add gateway ToJSON for events
- Update types JSON comments
- Upload to Hackage
- Rewrite rate-limiting loop to use a PSQueue for rate limited requests
- Add the two other auth options besides Bot: Client & Bearer
