# Changelog

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
