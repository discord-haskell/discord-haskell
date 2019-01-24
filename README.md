# discord-haskell

Please refer to `Getting Started` and `Notes` when 
relevant. Hours of debugging can save you 
minutes of reading.

Recent change: `master` branch has the potentially broken, most
recent commits, `stable` has the most recent working version.
Pull requests are automatically made against `master` and it's
nice to merge pull requests to test them.

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

6 Understand what's available to the bot. Rest API calls can modify
[Channels](https://discordapp.com/developers/docs/resources/channel#get-channel),
[Emoji](https://discordapp.com/developers/docs/resources/emoji#list-guild-emojis),
[Guilds](https://discordapp.com/developers/docs/resources/guild#get-guild),
etc. Most endpoints are covered with very similar names. `List Guild Emojis`
becomes `ListGuildEmojis`. You can use `:info` liberally on type constructors to
explore the ADTs.

[Gateway Events](https://discordapp.com/developers/docs/topics/gateway#commands-and-events-gateway-events)
provide the other source of info, using `nextEvent` and `sendCommand`. Use `:info` to explore `Event` and `GatewaySendable` ADTs.

7 Add this library to your dependencies. discord-haskell is on hackage
with strict version bounds to stackage lts-12.10. You can also use
the github repo.

```
# in stack.yaml (if using stack)
resolver: lts-12.10
extra-deps:
- git: git@github.com:aquarial/discord-haskell.git
  commit: <most recent stable commit>
  extra-dep: true

# in project.cabal
  build-depends:       base
                     , discord-haskell

```

## Notes

`loginRest` allows `restCall`. `loginRestGateway` allows `restCall`,
`nextEvent`, `sendCommand`, and `readCache`. **Use `loginRest` if you don't need the 
gateway**

Use `Control.Exception.finally` with `stopDiscord` to safely
kill background threads when running examples in ghci
(otherwise exit ghci and reopen to kill threads)

The examples will work on the `stable` branch. The `master` branch
has the most recent (potentially) breaking changes.

To get the format to use for Emoji, type `\:emoji:` into 
a discord chat. You should copy-paste that into the request. This
can be a bit finicky.  The equivalent of `:thumbsup::skin-tone-3:`
is `"👍\127997"` for example, and a custom emoji will look
like `<name:id_number>` or `name:id_number`

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

