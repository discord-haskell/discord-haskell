### Cache

The cache (`readCache`) is a work in progress.

The CurrentUser and Application fields are filled before the onStart handler is called.

Other fields are not filled in by default. If `RunDiscordOpts.discordEnableCache` is `true` then they will be filled in as the gateay receives events.

No rest requests are cached (yet, WIP).

Current source code is at [Discord.Internal.Gateway.Cache](../src/Discord/Internal/Gateway/Cache.hs)
