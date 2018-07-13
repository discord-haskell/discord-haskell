# discord-haskell

The examples will work on the `master` branch. The `dev` branch
has the most recent (potentially) breaking changes.

The library is usable for simple gateway & rest bots.
The two largest missing features are that
it's missing some gateway & rest types and
it can't lookup guilds/channels/messages by name

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
[examples/rest.hs](./examples/rest.hs), and
[examples/ping-pong.hs](./examples/ping-pong.hs) usually work

## Overview

[discord/Rest/HTTPS.hs](./src/Discord/Rest/HTTP.hs)
 provides a Chan that executes each http request it gets respecting the rate limits

[discord/Rest.hs](./src/Discord/Rest.hs) provides
an IO action that simplifies executing http requests

[discord/Gateway.hs](./src/Discord/Gateway.hs)
can construct a real time Chan of `Event`s the user can read from

## History

I started discord-haskell by forking
[discord.hs](https://github.com/jano017/Discord.hs), but
I have since rewritten all of the rest and gateway logic.

## TODO

In roughly the order I'm working on them:

- Rework gateway FromJSON parsing
- Add all gateway types (emoji removed, etc)
- Add      [Activity object](https://discordapp.com/developers/docs/topics/gateway#activity-object) for PresenceInfo in the Event ADT
- Add neat [permission adt](https://discordapp.com/developers/docs/topics/permissions) handling
- Add neat [presence adt](https://discordapp.com/developers/docs/topics/gateway#presence-update) handling
- Add gateway ToJSON for events
- Double check the REST request ADT matches the API
- Update channel types (fill out guildcategory)
- Update types JSON comments
- More helpful README (examples, how to depend it this package)
- Upload to Hackage
- Rewrite rate-limiting loop to use a PSQueue for rate limited requests
- Add the two other auth options besides Bot: Client & Bearer
