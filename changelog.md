# Changelog

Go to the [dev branch](https://github.com/aquarial/discord-haskell/blob/dev/changelog.md) for changes not merged into master yet

### 0.6.1

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
