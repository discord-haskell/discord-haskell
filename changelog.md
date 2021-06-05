# Changelog

View on github for newest version: https://github.com/aquarial/discord-haskell/blob/master/changelog.md

Discord API changes, so use the most recent version at all times

## master

## 1.8.8

Remove git artifacts from [examples/ping-pong.hs](https://github.com/aquarial/discord-haskell/issues/69)

## 1.8.7

Add [Stage channel](https://github.com/aquarial/discord-haskell/issues/68) and a catch-all Unknown channel so we stop crashing on new releases (?)

## 1.8.6

Add [missing fields](https://github.com/aquarial/discord-haskell/issues/67) to ChannelGuildCategory

## 1.8.5

 Fix examples/ping-pong.hs compilation error https://github.com/aquarial/discord-haskell/issues/65

## 1.8.4

[yutotakano](https://github.com/aquarial/discord-haskell/pull/64) Added discord replies type, and message constructor

## 1.8.3

Bot no longer disconnects randomly (hopefully)  https://github.com/aquarial/discord-haskell/issues/62

## 1.8.2

Added 'Competing' activity https://github.com/aquarial/discord-haskell/issues/61

Resend the last Activity settings on Resume fixing https://github.com/aquarial/discord-haskell/issues/60
## 1.8.1

Added `MessageReaction` to Message https://github.com/aquarial/discord-haskell/issues/56

## 1.8.0

Fixed [null parent_id on channel](https://github.com/aquarial/discord-haskell/issues/55)

## 1.7.0

[elikoga](https://github.com/aquarial/discord-haskell/pull/51) Changed to use `ReaderT` interface

[elikoga](https://github.com/aquarial/discord-haskell/pull/50) Fixed compiler warnings

Changed api url to new `discord.com`

## 1.6.1

Changed discordapp.com to discord.com in accordance with official discord policy 

[rexim](https://github.com/aquarial/discord-haskell/pull/41) Add `Emoji.user` field. Who uploaded the emoji

## 1.6.0

Add News Channel and StorePage Channel. Fix crash `Unknown channel type:5`

Add NSFW and UserRateLimit to `Channel` type

## 1.5.1

Fix `EditMessage` rest request, send JSON

## 1.5.0

[rexim](https://github.com/aquarial/discord-haskell/pull/35) Add `Read` instance for `Snowflake`

## 1.4.0

Rename `SubEmbed` to `EmbedPart`

New and improved Embed API: Add `CreateEmbed` record and `createEmbed :: CreateEmbed -> Embed`

`CreateEmbedImageUpload` implementation inspired by [Flutterlice](https://github.com/aquarial/discord-haskell/pull/32)

## 1.3.0

[PixeLinc](https://github.com/aquarial/discord-haskell/pull/33) Add `DeleteSingleReaction` rest-request, Add GuildId to `ReactinInfo`, Add `MESSAGE_REACTION_REMOVE_EMOJI` gateway event

`GetReactions` actually returns the User objects request

Rename `Ban` to `GuildBan`

Re-export UTCTime from `time` package

## 1.2.0

[MDeltaX](https://github.com/aquarial/discord-haskell/pull/27) Fixed typo: depreciated --> deprecated

[MDeltaX](https://github.com/aquarial/discord-haskell/pull/29) More consistency: RoleID --> RoleId

[MDeltaX](https://github.com/aquarial/discord-haskell/pull/29) Fix ModifyGuildRole: Post --> Patch && optional args

[Hippu](https://github.com/aquarial/discord-haskell/pull/31) Won't crash on integer-nonces in ChannelMessage-events (usually strings)

## 1.1.3

Minor improvements to rate-limiting like using newer `X-RateLimit-Reset-After` header

## 1.1.2

[michalrus](https://github.com/aquarial/discord-haskell/issues/25) Fix `DeleteGuildRole` parse exception

## 1.1.1

Fix ModifyGuildRolePositions results in 400 Bad Request issue

## 1.1.0

Upgrade req to 2.x major version.

## 1.0.0

Going through some major updates to the library. Expect types to change and things to break.

Compare the [old ping-pong](https://github.com/aquarial/discord-haskell/blob/20f7f8556823a754c76d01484118a5abf336530b/examples/ping-pong.hs)
to the [new ping-pong](https://github.com/aquarial/discord-haskell/blob/7eaa6ca068f945603de7f43f6f270c2dbecd3c85/examples/ping-pong.hs)

Added a few rest ADT types

## 0.8.4

[marcotoniut](https://github.com/aquarial/discord-haskell/pull/18) Improved changed Embed ADT to have optional fields, and improved two field names

Add `ModifyGuildMember`, `AddGuildMember`, `AddGuildMemberRole`, `AddGuildMemberRole`, `RemoveGuildmembeRole`, `GetGuildBan`, `GetGuildVanityURL` rest data types

## 0.8.3

Simplify Message Author from `Either WebhookId User` to `User`

Add `loginRestGatewayWithLog`

### 0.8.2

Hardcode CreateReaction delay so bots can add reactions 4 times faster

[MP2E](https://github.com/aquarial/discord-haskell/pull/14) Fixed parse error on GuildBanAdd + GuildBanRevoke: user\_object instead the whole object

### 0.8.1

[MP2E](https://github.com/aquarial/discord-haskell/pull/11) Fixed parse error on GuildRoleDelete: role_id instead of role

### 0.8.0

`MessageUpdate` does not contain a full Message object, just `ChannelId` `MessageId`

Message Author changed from `User` to `Either WebhookId User`

Add Webhook ADT

Add requests: GetInvite, DeleteInvite

UpdateStatusVoiceOpts takes Bool for Mute

`Unavailable` becomes `GuildUnavailable`

### 0.7.1

[t1m0thyj](https://github.com/aquarial/discord-haskell/pull/6/files) Typo in RequestGuildMemberOpts fields fixed.

[t1m0thyj](https://github.com/aquarial/discord-haskell/pull/6/files) Added Activity, ActivityType ADT

UpdateStatusTypes became UpdateStatusType (singular ADT)

[t1m0thyj](https://github.com/aquarial/discord-haskell/pull/7) Retry connection on 1001 websocket close

### 0.7.0

Snowflake -> named id

Add requests: ModifyChanPositions, CreateGuildChannel

Changed constructors of Channel to have prefix "Channel", isGuildChannel --> channelIsInGuild

Change Emoji Id ADTs

### 0.6.0

Add requests: CreateGuildEmoji, GroupDMRemoveRecipient, ModifyCurrentUser, EditChannelPermissions, CreateChannelInvite, GroupDMAddRecipient, ModifyGuild

restCall, readCache pass errors as an ADT, including underling http exceptions

Only add "Bot " prefix to secret token if it's not there

### 0.5.1

sendCommand with GatewaySendable types

### 0.5.0

restCall with Request types

nextEvent with Event types
