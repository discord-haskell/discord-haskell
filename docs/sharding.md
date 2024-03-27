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

--------------------------

Note: discord sends an event to all shards that meet the shard-formula criteria. So if a user manually specifies shards [(0,1), (2,3)] and an event happens to match both (because the guilid rshift 22 mod 3 happens to be 2) then the event is sent to both websocket connections.

--------------------------

For library review: note that connection order doesn't matter. even though the docs say this in bold, twice.
The link to employee saying this https://discord.com/channels/81384788765712384/381887113391505410/923709602342764604

