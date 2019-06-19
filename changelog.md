# Changelog

View on github for newest version: https://github.com/aquarial/discord-haskell/blob/master/changelog.md

## master

[marcotoniut](https://github.com/aquarial/discord-haskell/pull/18) Improved changed Embed ADT to have optional fields, and improved two field names

Add `AddGuildMember` rest data type

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
