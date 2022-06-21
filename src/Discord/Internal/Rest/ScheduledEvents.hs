{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Provides actions for Scheduled Event API
module Discord.Internal.Rest.ScheduledEvents
    ( ScheduledEventRequest(..)
    ) where
import           Data.Aeson                     ( ToJSON(toJSON) )
import           Discord.Internal.Rest.Prelude  ( JsonRequest(..)
                                                , Request
                                                    ( jsonRequest
                                                    , majorRoute
                                                    )
                                                , baseUrl
                                                )
import           Discord.Internal.Types.Prelude ( GuildId
                                                , ScheduledEventId
                                                )
import           Discord.Internal.Types.ScheduledEvents
                                                ( CreateScheduledEventData
                                                , ModifyScheduledEventData
                                                , ScheduledEvent
                                                , ScheduledEventUser
                                                )
import qualified Network.HTTP.Req              as R
import           Network.HTTP.Req               ( (/:), (/~) )

-- | Data constructor for requests.
-- See <https://discord.com/developers/docs/resources/guild-scheduled-event>
data ScheduledEventRequest a where
  -- | Gets all the Scheduled Events of a Guild
  ListScheduledEvents    ::GuildId
                         -> ScheduledEventRequest [ScheduledEvent]
  -- | Creates a new ScheduledEvent
  CreateScheduledEvent   ::GuildId
                         -> CreateScheduledEventData
                         -> ScheduledEventRequest ScheduledEvent
  -- | Gets the information about an Event
  GetScheduledEvent      ::GuildId
                         -> ScheduledEventId
                         -> ScheduledEventRequest ScheduledEvent
  -- | Modifies a Scheduled Event's information
  ModifyScheduledEvent   ::GuildId
                         -> ScheduledEventId
                         -> ModifyScheduledEventData
                         -> ScheduledEventRequest ScheduledEvent
  -- | Delete a ScheduledEvent
  DeleteScheduledEvent   ::GuildId
                         -> ScheduledEventId
                         -> ScheduledEventRequest ()
  -- | Gets the Users that subscribed to the event
  GetScheduledEventUsers ::GuildId
                         -> ScheduledEventId
                         -> ScheduledEventRequest [ScheduledEventUser]

sevEndpoint :: GuildId -> R.Url 'R.Https
sevEndpoint gid = baseUrl /: "guilds" /~ gid /: "scheduled-events"

instance Request (ScheduledEventRequest a) where
    majorRoute = const "scheduledEvent"
    jsonRequest rq = case rq of
        ListScheduledEvents gid  -> Get (sevEndpoint gid) mempty
        GetScheduledEvent gid ev -> Get (sevEndpoint gid /~ ev) mempty
        CreateScheduledEvent gid ev ->
            Post (sevEndpoint gid) (pure $ R.ReqBodyJson $ toJSON ev) mempty
        ModifyScheduledEvent gid evi ev -> Patch
            (sevEndpoint gid /~ evi)
            (pure $ R.ReqBodyJson $ toJSON ev)
            mempty
        DeleteScheduledEvent gid evi -> Delete (sevEndpoint gid /~ evi) mempty
        GetScheduledEventUsers gid evi ->
            Get (sevEndpoint gid /~ evi /: "users") mempty
