# Changelog

View on GitHub for the newest ChangeLog: https://github.com/discord-haskell/discord-haskell/blob/master/changelog.md

The Discord API constantly changes. This library issues updates when we implement new features added to the API or remove outdated functionalities. In order to interact with the Discord API safely and predictably, please update the library whenever there is a new version released.

## Unreleased

-

## 1.16.0

- [L0neGamer](https://github.com/discord-haskell/discord-haskell/pull/187) Switched StatusOpts to a list of activities

- [L0neGamer](https://github.com/discord-haskell/discord-haskell/pull/188) Dropped support for Aeson < 2.0.0 (see [here for migration guid](https://github.com/haskell/aeson/issues/881) and [here for why](https://frasertweedale.github.io/blog-fp/posts/2021-10-12-aeson-hash-flooding-protection.html))

- [aquarial](https://github.com/discord-haskell/discord-haskell/pull/190) Populate cache before onStart handler. Cache includes more app info

- [aquarial](https://github.com/discord-haskell/discord-haskell/pull/194) Export `ModalData` ADT internals


## 1.15.6

- [penelopeysm](https://github.com/discord-haskell/discord-haskell/pull/176) GHC 9.6 dependencies

- [penelopeysm](https://github.com/discord-haskell/discord-haskell/pull/179) and [penelopeysm](https://github.com/discord-haskell/discord-haskell/pull/181) Improving emoji support

- [penelopeysm](https://github.com/discord-haskell/discord-haskell/pull/182) Fix StartThreadNoMessage endpoint

- [yutotakano](https://github.com/discord-haskell/discord-haskell/pull/183) Slim down stack matrix build

## 1.15.5

- [Gregory1234](https://github.com/discord-haskell/discord-haskell/pull/173) Adding `global_name` field to the User object

## 1.15.4

- [matobet](https://github.com/discord-haskell/discord-haskell/pull/148) Adding GHC 9.2.* support

- [aquarial](https://github.com/discord-haskell/discord-haskell/pull/149) Slash command validation extends to numbers

- [0x3alex](https://github.com/discord-haskell/discord-haskell/pull/152) Permissions bit flags

- [yutotakano](https://github.com/discord-haskell/discord-haskell/pull/153) Extending CI testing

- [1Computer1](https://github.com/discord-haskell/discord-haskell/pull/154) Exporting some internal types

- [L0neGamer](https://github.com/discord-haskell/discord-haskell/pull/155) Select menu additions

- [L0neGamer](https://github.com/discord-haskell/discord-haskell/pull/158) Caching more information

- [tam-carre](https://github.com/discord-haskell/discord-haskell/pull/159) New example for interactions

- [XanderDJ](https://github.com/discord-haskell/discord-haskell/pull/164) New role permissions setup

- [chuahao](https://github.com/discord-haskell/discord-haskell/pull/168) Fixing parsing of permissions

- [chuahao](https://github.com/discord-haskell/discord-haskell/pull/169) Adding role icon to `ModifyGuildRoleOpts`

- [chuahao](https://github.com/discord-haskell/discord-haskell/pull/170) Add utilities to measure the latency to discord

## 1.15.3

- [L0neGamer](https://github.com/discord-haskell/discord-haskell/pull/145) Fixing behind the scenes for hackage

## 1.15.2

- [L0neGamer](https://github.com/discord-haskell/discord-haskell/pull/143) Adding some utility and fixing some versions in place

## 1.15.1

- [Geometer1729](https://github.com/discord-haskell/discord-haskell/pull/141) Fixing a bug in localization code

## 1.15.0

- [Annwan](https://github.com/discord-haskell/discord-haskell/pull/137) Implemented optional localization for application commands. `[..]LocalizedName` and `[..]LocalizedDescription` fields have been added to many ADTs ([Discord documentation](https://discord.com/developers/docs/interactions/application-commands#localization))

- [L0neGamer](https://github.com/discord-haskell/discord-haskell/pull/136) Removed `applicationCommandDefaultPermission` from `ApplicationCommand`, replaced it with `applicationCommandDefaultMemberPermissions` and `applicationCommandDMPermission` ([Discord changelog](https://discord.com/developers/docs/change-log#updated-command-permissions))

- [yutotakano](https://github.com/discord-haskell/discord-haskell/pull/135) Implemented session-specific Resume URLs for the Gateway internally, which will prevent disconnects in the future ([Discord changelog](https://discord.com/developers/docs/change-log#sessionspecific-gateway-resume-urls)). Also removed the deprecated list of private channels received in Ready event.

- [L0neGamer](https://github.com/discord-haskell/discord-haskell/pull/133) Implemented maximum and minimum string lengths for application command options ([Discord changelog](https://discord.com/developers/docs/change-log#min-and-max-length-for-command-options)). Also implemented calculated context permissions for interaction payloads ([Discord changelog](https://discord.com/developers/docs/change-log#calculated-permissions-in-interaction-payloads))

- [L0neGamer](https://github.com/discord-haskell/discord-haskell/pull/132) Simplified internals of JSON creation using `objectFromMaybes` and `.=?`. Support `aeson-2.0`

- [yutotakano](https://github.com/discord-haskell/discord-haskell/pull/134) Loosened some acceptable version bounds for `http-client`, `req` and `http-api-data`, that were added with 1.14.0

## 1.14.0

- [degustaf](https://github.com/discord-haskell/discord-haskell/pull/131) Add `Exception` instance for `RestCallErrorCode`

- [yutotakano](https://github.com/discord-haskell/discord-haskell/pull/124) Replace JuicyPixels image parsing with a mimetype check. Make image handling consistent: use `parseStickerImage` fro sticker images. Use `parseAvatarImage` for avatars.

- [L0neGamer](https://github.com/discord-haskell/discord-haskell/pull/123/files) Make webhook API smaller, each constructor takes in a `Maybe WebhookToken`. Passing `Nothing` will continue to work as normal.

- [Annwan](https://github.com/discord-haskell/discord-haskell/pull/123) Huge documentation flourish. Removed deprecated AppCommandPermissions func & fix presences typo

- [yutotakano](https://github.com/discord-haskell/discord-haskell/pull/121) Replace `OverwriteId` with `Either RoleId UserId` in `ChannelPermission` requests, and remove the `type` field from `ChannelPermissionsOpts`

## 1.13.0

- [L0neGamer](https://github.com/discord-haskell/discord-haskell/pull/117) Shorten ApplicationCommand names! To update search [the pull-request](https://github.com/discord-haskell/discord-haskell/pull/117/files) for what the names are replaced with

- [L0neGamer](https://github.com/discord-haskell/discord-haskell/pull/116) Typesafe Snowflakes (guildid, channelid, userid, etcid)

- Improve `restCall` type error messages https://github.com/discord-haskell/discord-haskell/issues/102

## 1.12.5

- [Annwan](https://github.com/discord-haskell/discord-haskell/pull/109) Add `ScheduledEvent` rest API

- [L0neGamer](https://github.com/discord-haskell/discord-haskell/pull/110) Add stickers API

- [L0neGamer](https://github.com/discord-haskell/discord-haskell/pull/111) Add ModifyGuildMember 'timeout' option

## 1.12.4

- Library won't crash if something fails to parse. Errors are printed to the log

## 1.12.3

- Add another CreateMessage flag option, stop crashing on unknown flags.

## 1.12.2

- [L0neGamer](https://github.com/discord-haskell/discord-haskell/pull/107) `EditMessage` takes full `MessageDetailedOpts` (instead of Embed)

- Removed `CreateMessageUploadFile` (use `CreateMessageDetailed { MessageDetailedOpts { messageDetailedFile } }`)

## 1.12.1

- [L0neGamer](https://github.com/discord-haskell/discord-haskell/pull/103) Add threads, switch api to V10, Update Guild data fields

- [L0neGamer](https://github.com/discord-haskell/discord-haskell/pull/104) Add model interaction and components

## 1.12.0

- [L0neGamer](https://github.com/discord-haskell/discord-haskell/pull/96) breaking changes and fixes to application commands, interactions, and components, and updates elsewhere

## 1.11.0

- [L0neGamer](https://github.com/discord-haskell/discord-haskell/pull/88) did a LOT of work wrangling the discord API for interactions and commands!

- [L0neGamer](https://github.com/discord-haskell/discord-haskell/pull/94) fixed a parse error with webhooktoken

- Rename fields `messageText` -> `messageContent`, `messageChannel` -> `messageChannelId`

## 1.10.0

- [drewolson](https://github.com/discord-haskell/discord-haskell/pull/80) allows parsing an optional guild region

- [L0neGamer](https://github.com/discord-haskell/discord-haskell/pull/82) add 'animated' flag for Emoji

- Removed `CreateGuild` rest call! You can only do it if your bot is in fewer than 10 guilds, and it's [a pain to support](https://discord.com/developers/docs/resources/guild#create-guild). Just do it manually.

- Added `Read` instance to complement `Show` for lots of types. Removed ToJSON for `Channel`. 

## 1.9.1

- Add [color attribute for CreateEmbed](https://github.com/discord-haskell/discord-haskell/issues/78)

- Rewrite [EventLoop.hs](https://github.com/discord-haskell/discord-haskell/issues/70) to be easier to modify

- Rename a bunch of internal handles so they have more consistent names

## 1.8.9

- Handle both aeson 1.0 and 2.0 [(breaking changes broke builds)](https://github.com/discord-haskell/discord-haskell/issues/77)

- Simplify [some examples](https://github.com/discord-haskell/discord-haskell/issues/71)

## 1.8.8

- Remove git artifacts from [examples/ping-pong.hs](https://github.com/discord-haskell/discord-haskell/issues/69)

## 1.8.7

- Add [Stage channel](https://github.com/discord-haskell/discord-haskell/issues/68) and a catch-all Unknown channel so we stop crashing on new releases (?)

## 1.8.6

- Add [missing fields](https://github.com/discord-haskell/discord-haskell/issues/67) to ChannelGuildCategory

## 1.8.5

- Fix examples/ping-pong.hs compilation error https://github.com/discord-haskell/discord-haskell/issues/65

## 1.8.4

- [yutotakano](https://github.com/discord-haskell/discord-haskell/pull/64) Added discord replies type, and message constructor

## 1.8.3

- Bot no longer disconnects randomly (hopefully)  https://github.com/discord-haskell/discord-haskell/issues/62

## 1.8.2

- Added 'Competing' activity https://github.com/discord-haskell/discord-haskell/issues/61

- Resend the last Activity settings on Resume fixing https://github.com/discord-haskell/discord-haskell/issues/60
## 1.8.1

- Added `MessageReaction` to Message https://github.com/discord-haskell/discord-haskell/issues/56

## 1.8.0

- Fixed [null parent_id on channel](https://github.com/discord-haskell/discord-haskell/issues/55)

## 1.7.0

- [elikoga](https://github.com/discord-haskell/discord-haskell/pull/51) Changed to use `ReaderT` interface

- [elikoga](https://github.com/discord-haskell/discord-haskell/pull/50) Fixed compiler warnings

- Changed api url to new `discord.com`

## 1.6.1

- Changed discordapp.com to discord.com in accordance with official discord policy 

- [rexim](https://github.com/discord-haskell/discord-haskell/pull/41) Add `Emoji.user` field. Who uploaded the emoji

## 1.6.0

- Add News Channel and StorePage Channel. Fix crash `Unknown channel type:5`

- Add NSFW and UserRateLimit to `Channel` type

## 1.5.1

- Fix `EditMessage` rest request, send JSON

## 1.5.0

- [rexim](https://github.com/discord-haskell/discord-haskell/pull/35) Add `Read` instance for `Snowflake`

## 1.4.0

- Rename `SubEmbed` to `EmbedPart`

- New and improved Embed API: Add `CreateEmbed` record and `createEmbed :: CreateEmbed -> Embed`

- `CreateEmbedImageUpload` implementation inspired by [Flutterlice](https://github.com/discord-haskell/discord-haskell/pull/32)

## 1.3.0

- [PixeLinc](https://github.com/discord-haskell/discord-haskell/pull/33) Add `DeleteSingleReaction` rest-request, Add GuildId to `ReactinInfo`, Add `MESSAGE_REACTION_REMOVE_EMOJI` gateway event

- `GetReactions` actually returns the User objects request

- Rename `Ban` to `GuildBan`

- Re-export UTCTime from `time` package

## 1.2.0

- [MDeltaX](https://github.com/discord-haskell/discord-haskell/pull/27) Fixed typo: depreciated --> deprecated

- [MDeltaX](https://github.com/discord-haskell/discord-haskell/pull/29) More consistency: RoleID --> RoleId

- [MDeltaX](https://github.com/discord-haskell/discord-haskell/pull/29) Fix ModifyGuildRole: Post --> Patch && optional args

- [Hippu](https://github.com/discord-haskell/discord-haskell/pull/31) Won't crash on integer-nonces in ChannelMessage-events (usually strings)

## 1.1.3

- Minor improvements to rate-limiting like using newer `X-RateLimit-Reset-After` header

## 1.1.2

- [michalrus](https://github.com/discord-haskell/discord-haskell/issues/25) Fix `DeleteGuildRole` parse exception

## 1.1.1

- Fix ModifyGuildRolePositions results in 400 Bad Request issue

## 1.1.0

- Upgrade req to 2.x major version.

## 1.0.0

- Going through some major updates to the library. Expect types to change and things to break.

- Compare the [old ping-pong](https://github.com/discord-haskell/discord-haskell/blob/20f7f8556823a754c76d01484118a5abf336530b/examples/ping-pong.hs)
to the [new ping-pong](https://github.com/discord-haskell/discord-haskell/blob/7eaa6ca068f945603de7f43f6f270c2dbecd3c85/examples/ping-pong.hs)

- Added a few rest ADT types

## 0.8.4

- [marcotoniut](https://github.com/discord-haskell/discord-haskell/pull/18) Improved changed Embed ADT to have optional fields, and improved two field names

- Add `ModifyGuildMember`, `AddGuildMember`, `AddGuildMemberRole`, `AddGuildMemberRole`, `RemoveGuildmembeRole`, `GetGuildBan`, `GetGuildVanityURL` rest data types

## 0.8.3

- Simplify Message Author from `Either WebhookId User` to `User`

- Add `loginRestGatewayWithLog`

### 0.8.2

- Hardcode CreateReaction delay so bots can add reactions 4 times faster

- [MP2E](https://github.com/discord-haskell/discord-haskell/pull/14) Fixed parse error on GuildBanAdd + GuildBanRevoke: user\_object instead the whole object

### 0.8.1

- [MP2E](https://github.com/discord-haskell/discord-haskell/pull/11) Fixed parse error on GuildRoleDelete: role_id instead of role

### 0.8.0

- `MessageUpdate` does not contain a full Message object, just `ChannelId` `MessageId`

- Message Author changed from `User` to `Either WebhookId User`

- Add Webhook ADT

- Add requests: GetInvite, DeleteInvite

- UpdateStatusVoiceOpts takes Bool for Mute

- `Unavailable` becomes `GuildUnavailable`

### 0.7.1

- [t1m0thyj](https://github.com/discord-haskell/discord-haskell/pull/6/files) Typo in RequestGuildMemberOpts fields fixed.

- [t1m0thyj](https://github.com/discord-haskell/discord-haskell/pull/6/files) Added Activity, ActivityType ADT

- UpdateStatusTypes became UpdateStatusType (singular ADT)

- [t1m0thyj](https://github.com/discord-haskell/discord-haskell/pull/7) Retry connection on 1001 websocket close

### 0.7.0

- Snowflake -> named id

- Add requests: ModifyChanPositions, CreateGuildChannel

- Changed constructors of Channel to have prefix "Channel", isGuildChannel --> channelIsInGuild

- Change Emoji Id ADTs

### 0.6.0

- Add requests: CreateGuildEmoji, GroupDMRemoveRecipient, ModifyCurrentUser, EditChannelPermissions, CreateChannelInvite, GroupDMAddRecipient, ModifyGuild

- restCall, readCache pass errors as an ADT, including underling http exceptions

- Only add "Bot " prefix to secret token if it's not there

### 0.5.1

- sendCommand with GatewaySendable types

### 0.5.0

- restCall with Request types

- nextEvent with Event types
