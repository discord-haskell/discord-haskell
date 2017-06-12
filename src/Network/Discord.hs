-- | Provides core Discord functionallity. 
{-# LANGUAGE DataKinds, TypeOperators, KindSignatures, RankNTypes, FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses, GADTs, TypeFamilies, FlexibleContexts #-}
module Network.Discord
  ( module Network.Discord
  , module Network.Discord.Framework
  , module Network.Discord.Gateway
  , module Network.Discord.Rest
  , module Network.Discord.Types
  ) where
  import Network.Discord.Framework
  import Network.Discord.Rest
  import Network.Discord.Types
  import Network.Discord.Gateway

  import Control.Monad (mzero)
  import Control.Applicative ((<|>))
  import Data.Proxy
  import GHC.TypeLits hiding ((:<>:))

  class (DiscordAuth m, Event ~ Domain f, () ~ Codomain f, EventMap f (DiscordApp m))
    => EventHandler f m

  runBot :: (DiscordAuth m, EventHandler f m) => Proxy (m f) -> IO ()
  runBot p = runGateway gatewayUrl $ (DiscordApp (\_ e -> return e) >>= go p) <|> return ()
    where
      split :: Proxy (a b) -> (Proxy a, Proxy b)
      split _ = (Proxy, Proxy)
      go :: EventHandler f m => Proxy (m f) -> Event -> DiscordApp m ()
      go p' = let (_, b) = split p' in mapEvent b

  data ReadyEvent
  
  instance DiscordAuth m => EventMap ReadyEvent (DiscordApp m) where
    type Domain   ReadyEvent = Event
    type Codomain ReadyEvent = Init

    mapEvent _ (Ready e) = return e
    mapEvent _ _ = mzero

  data ResumedEvent

  instance DiscordAuth m =>  EventMap ResumedEvent (DiscordApp m) where
    type Domain   ResumedEvent = Event
    type Codomain ResumedEvent = Object

    mapEvent _ (Resumed e) = return e
    mapEvent _ _ = mzero

  data ChannelCreateEvent

  instance DiscordAuth m =>  EventMap ChannelCreateEvent (DiscordApp m) where
    type Domain   ChannelCreateEvent = Event
    type Codomain ChannelCreateEvent = Channel

    mapEvent _ (ChannelCreate e) = return e
    mapEvent _ _ = mzero
  
  data ChannelUpdateEvent
  
  instance DiscordAuth m =>  EventMap ChannelUpdateEvent (DiscordApp m) where
    type Domain   ChannelUpdateEvent = Event
    type Codomain ChannelUpdateEvent = Channel

    mapEvent _ (ChannelUpdate e) = return e
    mapEvent _ _ = mzero

  data ChannelDeleteEvent

  instance DiscordAuth m =>  EventMap ChannelDeleteEvent (DiscordApp m) where
    type Domain   ChannelDeleteEvent = Event
    type Codomain ChannelDeleteEvent = Channel

    mapEvent _ (ChannelDelete e) = return e
    mapEvent _ _ = mzero

  data GuildCreateEvent

  instance DiscordAuth m =>  EventMap GuildCreateEvent (DiscordApp m) where
    type Domain   GuildCreateEvent = Event
    type Codomain GuildCreateEvent = Guild

    mapEvent _ (GuildCreate e) = return e
    mapEvent _ _ = mzero

  data GuildUpdateEvent

  instance DiscordAuth m =>  EventMap GuildUpdateEvent (DiscordApp m) where
    type Domain   GuildUpdateEvent = Event
    type Codomain GuildUpdateEvent = Guild

    mapEvent _ (GuildUpdate e) = return e
    mapEvent _ _ = mzero

  data GuildDeleteEvent

  instance DiscordAuth m =>  EventMap GuildDeleteEvent (DiscordApp m) where
    type Domain   GuildDeleteEvent = Event
    type Codomain GuildDeleteEvent = Guild

    mapEvent _ (GuildDelete e) = return e
    mapEvent _ _ = mzero

  data GuildBanAddEvent

  instance DiscordAuth m =>  EventMap GuildBanAddEvent (DiscordApp m) where
    type Domain   GuildBanAddEvent = Event
    type Codomain GuildBanAddEvent = Member

    mapEvent _ (GuildBanAdd e) = return e
    mapEvent _ _ = mzero

  data GuildBanRemoveEvent

  instance DiscordAuth m =>  EventMap GuildBanRemoveEvent (DiscordApp m) where
    type Domain   GuildBanRemoveEvent = Event
    type Codomain GuildBanRemoveEvent = Member

    mapEvent _ (GuildBanRemove e) = return e
    mapEvent _ _ = mzero

  data GuildEmojiUpdateEvent

  instance DiscordAuth m =>  EventMap GuildEmojiUpdateEvent (DiscordApp m) where
    type Domain   GuildEmojiUpdateEvent = Event
    type Codomain GuildEmojiUpdateEvent = Object

    mapEvent _ (GuildEmojiUpdate e) = return e
    mapEvent _ _ = mzero

  data GuildIntegrationsUpdateEvent

  instance DiscordAuth m =>  EventMap GuildIntegrationsUpdateEvent (DiscordApp m) where
    type Domain   GuildIntegrationsUpdateEvent = Event
    type Codomain GuildIntegrationsUpdateEvent = Object

    mapEvent _ (GuildIntegrationsUpdate e) = return e
    mapEvent _ _ = mzero

  data GuildMemberAddEvent

  instance DiscordAuth m =>  EventMap GuildMemberAddEvent (DiscordApp m) where
    type Domain   GuildMemberAddEvent = Event
    type Codomain GuildMemberAddEvent = Member

    mapEvent _ (GuildMemberAdd e) = return e
    mapEvent _ _ = mzero

  data GuildMemberRemoveEvent

  instance DiscordAuth m =>  EventMap GuildMemberRemoveEvent (DiscordApp m) where
    type Domain   GuildMemberRemoveEvent = Event
    type Codomain GuildMemberRemoveEvent = Member

    mapEvent _ (GuildMemberRemove e) = return e
    mapEvent _ _ = mzero

  data GuildMemberUpdateEvent

  instance DiscordAuth m =>  EventMap GuildMemberUpdateEvent (DiscordApp m) where
    type Domain   GuildMemberUpdateEvent = Event
    type Codomain GuildMemberUpdateEvent = Member

    mapEvent _ (GuildMemberUpdate e) = return e
    mapEvent _ _ = mzero

  data GuildMemberChunkEvent

  instance DiscordAuth m =>  EventMap GuildMemberChunkEvent (DiscordApp m) where
    type Domain   GuildMemberChunkEvent = Event
    type Codomain GuildMemberChunkEvent = Object

    mapEvent _ (GuildMemberChunk e) = return e
    mapEvent _ _ = mzero

  data GuildRoleCreateEvent

  instance DiscordAuth m =>  EventMap GuildRoleCreateEvent (DiscordApp m) where
    type Domain   GuildRoleCreateEvent = Event
    type Codomain GuildRoleCreateEvent = Object

    mapEvent _ (GuildRoleCreate e) = return e
    mapEvent _ _ = mzero

  data GuildRoleUpdateEvent

  instance DiscordAuth m =>  EventMap GuildRoleUpdateEvent (DiscordApp m) where
    type Domain   GuildRoleUpdateEvent = Event
    type Codomain GuildRoleUpdateEvent = Object

    mapEvent _ (GuildRoleUpdate e) = return e
    mapEvent _ _ = mzero

  data GuildRoleDeleteEvent

  instance DiscordAuth m =>  EventMap GuildRoleDeleteEvent (DiscordApp m) where
    type Domain   GuildRoleDeleteEvent = Event
    type Codomain GuildRoleDeleteEvent = Object

    mapEvent _ (GuildRoleDelete e) = return e
    mapEvent _ _ = mzero

  data MessageCreateEvent

  instance DiscordAuth m =>  EventMap MessageCreateEvent (DiscordApp m) where
    type Domain   MessageCreateEvent = Event
    type Codomain MessageCreateEvent = Message

    mapEvent _ (MessageCreate e) = return e
    mapEvent _ _ = mzero
  
  data MessageUpdateEvent

  instance DiscordAuth m =>  EventMap MessageUpdateEvent (DiscordApp m) where
    type Domain   MessageUpdateEvent = Event
    type Codomain MessageUpdateEvent = Message

    mapEvent _ (MessageUpdate e) = return e
    mapEvent _ _ = mzero

  data MessageDeleteEvent

  instance DiscordAuth m =>  EventMap MessageDeleteEvent (DiscordApp m) where
    type Domain   MessageDeleteEvent = Event
    type Codomain MessageDeleteEvent = Object

    mapEvent _ (MessageDelete e) = return e
    mapEvent _ _ = mzero

  data MessageDeleteBulkEvent

  instance DiscordAuth m =>  EventMap MessageDeleteBulkEvent (DiscordApp m) where
    type Domain   MessageDeleteBulkEvent = Event
    type Codomain MessageDeleteBulkEvent = Object

    mapEvent _ (MessageDeleteBulk e) = return e
    mapEvent _ _ = mzero

  data PresenceUpdateEvent

  instance DiscordAuth m =>  EventMap PresenceUpdateEvent (DiscordApp m) where
    type Domain   PresenceUpdateEvent = Event
    type Codomain PresenceUpdateEvent = Object

    mapEvent _ (PresenceUpdate e) = return e
    mapEvent _ _ = mzero

  data TypingStartEvent

  instance DiscordAuth m =>  EventMap TypingStartEvent (DiscordApp m) where
    type Domain   TypingStartEvent = Event
    type Codomain TypingStartEvent = Object

    mapEvent _ (TypingStart e) = return e
    mapEvent _ _ = mzero

  data UserSettingsUpdateEvent

  instance DiscordAuth m =>  EventMap UserSettingsUpdateEvent (DiscordApp m) where
    type Domain   UserSettingsUpdateEvent = Event
    type Codomain UserSettingsUpdateEvent = Object

    mapEvent _ (UserSettingsUpdate e) = return e
    mapEvent _ _ = mzero

  data UserUpdateEvent

  instance DiscordAuth m =>  EventMap UserUpdateEvent (DiscordApp m) where
    type Domain   UserUpdateEvent = Event
    type Codomain UserUpdateEvent = Object

    mapEvent _ (UserUpdate e) = return e
    mapEvent _ _ = mzero

  data VoiceStateUpdateEvent

  instance DiscordAuth m =>  EventMap VoiceStateUpdateEvent (DiscordApp m) where
    type Domain   VoiceStateUpdateEvent = Event
    type Codomain VoiceStateUpdateEvent = Object

    mapEvent _ (VoiceStateUpdate e) = return e
    mapEvent _ _ = mzero

  data VoiceServerUpdateEvent

  instance DiscordAuth m => EventMap VoiceServerUpdateEvent (DiscordApp m) where
    type Domain   VoiceServerUpdateEvent = Event
    type Codomain VoiceServerUpdateEvent = Object

    mapEvent _ (VoiceServerUpdate e) = return e
    mapEvent _ _ = mzero

  data OtherEvent (a :: Symbol)

  instance (DiscordAuth m, KnownSymbol e) => EventMap (OtherEvent e) (DiscordApp m) where
    type Domain   (OtherEvent e) = Event
    type Codomain (OtherEvent e) = Object

    mapEvent p (UnknownEvent s e)
      | s == event p = return e
      | otherwise    = mzero
      where
        event :: KnownSymbol a => Proxy (OtherEvent a) -> String
        event op = symbolVal $ eventName op
        eventName :: Proxy (OtherEvent a) -> Proxy a
        eventName _ = Proxy
    mapEvent _ _ = mzero
