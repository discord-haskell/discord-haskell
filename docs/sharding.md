### Sharding

Sharding is a way to split up incoming data into multiple parts so that each part only handles a fraction of the data.

When a discord bot connects to the gateway, it can declare which 'shard' it is, and only have to handle a fraction of the incoming data.

Discord recommends sharding a bot so that each shard will handle 1,000 guilds.

Internally the library will use [get getaway](https://discord.com/developers/docs/topics/gateway#get-gateway-bot) to start the recommended number of shards. These shards will run multi-threaded. **Users will only have to manually select sharding when the computer the bot is running on cannot handle the total number of incoming events**.

If a bot is running into these scaling limits here are some things to look into:
- remove unnecessary intents from the `RunDiscordOpts.discordGatewayIntent` to reduce events sent
- use application-commands as much as possible
- startup another computer and run half the shards on one (by specifying `RunDiscordOpts.discordSharding`)

For information on how to specify sharding use https://discord.com/developers/docs/topics/gateway#sharding
