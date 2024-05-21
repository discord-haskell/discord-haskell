
Problem: i have a jank mvar setup for letting each shard hread know it's ok to connect.
problem: it doesn't re-lookup getbot in order to use the latest concurrence information.

need to pass rest into the gateway so it can lookup the 


could be some race conditions around gatewayHandleLastStatus.
It's sent to server each time the thread reconnectes.
The user sends to a thread with setStatus in Discord.hs
but if two different eventloop threads get different
  setStatus events, then each reconnect they'll fight over the
  status?

remember that eventchan should be dup'd once from the cachechan.
make sure to do this pretty in discord.hs

--------------


Completed version of https://github.com/discord-haskell/discord-haskell/pull/172, ready for review.

Sharding documentation is https://discord.com/developers/docs/topics/gateway#sharding

This PR will default to using the recommend shards given from discord using the rest call https://discord.com/developers/docs/topics/gateway#get-gateway-bot, but also provides a `runDiscordOpts` for specifying explicitly which shards.

This PR implements a simple version of rate-limiting connections: allow a shard to connect every 5 seconds. This is slower than the maximum of `session_start_limit.max_concurrency`, but the implementation is much simpler at little cost. 

For example: the rust library serenity recently merged PR https://github.com/serenity-rs/serenity/pull/2661 for using `max_concurrency`, but it hasn't made it to main yet. So all the released versions are using the simpler version of 1once every 5 seconds.

Sharding isn't even recommended until a bot is in 2500 guilds, so users are unlikely to run into this at all. 




Cache needs R.User and R.Application to intialize

ShardManager needs R.GatewayBot to decide shards & websocket

restCall needs cache, and restloop

ChangeShards (of type DiscordSharding -> Gateway


User things:
  RunDiscordOpts                                (things like discordOnEvent :: Event -> DiscordHandler)
  runDiscord :: RunDiscordOpts -> IO T.Text
  restCall :: Request -> DiscordHandler (Either) -- should use cache
