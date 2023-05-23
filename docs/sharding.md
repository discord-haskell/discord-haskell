### Sharding

Sharding means splitting up data when it gets too large for a single process.

When a discord bot is in a large number of guilds then the events will come in faster than a single websocket can handle. Discord docs recommend sharding a bot so each shard handles 1,000 guilds.

Internally the library will use [get getaway](https://discord.com/developers/docs/topics/gateway#get-gateway-bot) to start the recommended number of shards start the recommended number of websockets on startup.

discord-haskell will handle sharding across multiple processess on the same computer. However if the number of events becomes large enough that one computer cannot keep up with events, then the library user will have to start copies of the bot on separate computers and manually select the shard info. 

- https://discordjs.guide/sharding/#how-does-sharding-work
  - Some library style docs on sharding
  
- https://discord.com/developers/docs/topics/gateway#sharding
  - Discord docs on sharding

- https://github.com/jberryman/chan-benchmarks
  - adding and removing items from a chan is not a performance bottleneck for events.


Some research questions:
- How do ratelimits work with multiple shards?
- If the library is locally sharded into multiple connections, can it send websocket commands to any one? Any reason to allow preferences?
- what happens with overlapping shards: say `[0,2][1,2]` and `[0,3][1,3][2,3]`. Would an event be sent to both copies? One of them randomly?
