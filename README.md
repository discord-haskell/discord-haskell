# Discord.hs

## REWORK 2

Forked from [discord.hs](https://github.com/jano017/Discord.hs/tree/rework)

Rewrote:
- discord-gateway
- discord-rest

Mostly Unchanged
- discord-types
- rest api data types

## Getting Started

1 create bot, get clientid and secret
<https://discordapp.com/developers/applications/me>

2 Figure out what permissions you need
<https://discordapi.com/permissions.html>

3 invite bot to server
<https://discordapp.com/oauth2/authorize?client_id=CLIENT_ID&scope=bot&permissions=PERMISSIONS>

4 Connect to the gateway once in order to send CreateMessage events
