# discord-haskell

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

[discord-rest/Rest/HTTPS.hs](./discord-rest/src/Network/Discord/Rest/HTTP.hs)
 provides a Chan that executes each http request it gets respecting the rate limits

[discord-rest/Rest.hs](./discord-rest/src/Network/Discord/Rest.hs) provides
an IO action that simplifies executing http requests

[discord-gateway/Gateway.hs](./discord-gateway/src/Network/Discord/Gateway.hs)
can construct a real time Chan of `Event`s the user can read from

## History

I started discord-haskell by forking
[discord.hs](https://github.com/jano017/Discord.hs), but
I have since rewritten all of the rest and gateway logic.

## TODO

In roughly the order I'm working on them:

- Rework gateway FromJSON parsing
- Reconnect to gateway if the InvalidSession says True
- Add all gateway types (emoji removed, etc)
- Add gateway ToJSON for events
- Double check the REST request ADT matches the API
- Update channel types (fill out guildcategory)
- Update types JSON, removing strict and unpack
- Update types JSON to use 'withObject'
- Update types JSON comments
- More helpful README (examples, how to depend it this)
- Upload to Hackage
- Rewrite rate-limiting loop to use a PSQueue for rate limited requests
- Add the two other auth options besides Bot: Client & Bearer
