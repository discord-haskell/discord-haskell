
### TODO

#### Cache

Cache is a TODO at the moment.

A cache is nice for a user to query, and we could do some automatic RestCall response caching for free performance.

https://github.com/aquarial/discord-haskell/issues/44 wants to access the roles (in a `GuildMember` object) of the user who sent a `CreateMessage` event. However it only contains a `User` object. Need a separate RestCall to get the roles.

https://github.com/aquarial/discord-haskell/issues/89 asks that a user can put their own data in the cache and access it.

#### Higher level bot interface? easier to add state and stuff

https://github.com/aquarial/discord-haskell/blob/master/examples/state-counter.hs

https://github.com/aquarial/discord-haskell/issues/42 and https://github.com/aquarial/discord-haskell/issues/81 ask about how to store state in between event handler calls.

https://github.com/aquarial/discord-haskell/issues/63 asks for docs on how to deploy a bot to heroku.

The [state-counter.hs`](../examples/state-counter.hs) example shows how to increment a count between eventHandlers, and persist state to a file.
