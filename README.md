# Discord.hs

## Getting Started

1 Create a bot to get a clientid and secret
<https://discordapp.com/developers/applications/me>

2 Figure out what permissions you need
<https://discordapi.com/permissions.html>

3 Invite the bot to a server
<https://discordapp.com/oauth2/authorize?client_id=CLIENT_ID&scope=bot&permissions=PERMISSIONS>

4 Connect to the gateway once in order to send CreateMessage events

5 Look at the examples. [examples/gateway.hs](./examples/gateway.hs)
and [examples/rest.hs](./examples/rest.hs) work as of now

## Overview

The library core is based on Chans.


For the rest api
[discord-rest/HTTPS.hs](./discord-rest/src/Network/Discord/Rest/HTTP.hs)
executes each http request it gets respecting the ratelimits
[discord-rest/Rest.hs](./discord-rest/src/Network/Discord/Rest.hs) provides
a cleaner interface to the Chan

For the gateway api
[discord-gateway/Gateway.hs](./discord-gateway/src/Network/Discord/Gateway.hs)
can construct a Chan of `Event`s.

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

- Suport gateway RESUME events properly
- Cleaner interface to gateway that constructs the Chan
- Verify each REST request is sent properly
- Rewrite rate-limiting loop to use a PSQueue for rate limited requests

