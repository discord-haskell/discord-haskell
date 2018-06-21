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

5 Look at the examples. [examples/gateway.hs](./examples/gateway.hs)
and [examples/rest.hs](./examples/rest.hs) are usually kept in a working state

## Overview

[discord-rest/Rest/HTTPS.hs](./discord-rest/src/Network/Discord/Rest/HTTP.hs)
 provides a Chan that executes each http request it gets respecting the rate limits

[discord-rest/Rest.hs](./discord-rest/src/Network/Discord/Rest.hs) provides
an IO action that simplifies executing http requests

[discord-gateway/Gateway.hs](./discord-gateway/src/Network/Discord/Gateway.hs)
can construct a real time Chan of `Event`s the user can read from

## History

I forked [discord.hs](https://github.com/jano017/Discord.hs) and
rewrote the core logic of the api.

Rewritten:
- discord-gateway
- discord-rest

Mostly Unchanged
- discord-types
- rest api data types

## TODO

In roughly the order I'm working on them:

- Rework gateway FromJSON parsing
- Reconnect to gateway if the InvalidSession says True
- Add all gateway types (emoji removed, etc)
- Add gateway ToJSON for events
- Double check the REST request ADT matches the API
- Cleaner interface to gateway that constructs the Chan
- More helpful README
- Upload to Hackage
- Rewrite rate-limiting loop to use a PSQueue for rate limited requests
- Add the two other auth options besidees Bot: Client & Bearer
