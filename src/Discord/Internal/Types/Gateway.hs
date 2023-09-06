{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE RecordWildCards #-}

-- | Data structures needed for interfacing with the Websocket
--   Gateway
module Discord.Internal.Types.Gateway where

import System.Info

import qualified Data.Text as T
import Data.Time (UTCTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Data.Aeson
import Data.Aeson.Types
import Data.Default (Default, def)
import Data.Maybe (fromMaybe)
import Data.Functor
import Text.Read (readMaybe)

import Discord.Internal.Types.Prelude
import Discord.Internal.Types.Events
import Discord.Internal.Types.Guild (Activity (..))

-- | Messages that can be sent by gateway to the library
data GatewayReceivable
  = Dispatch EventInternalParse Integer
  | HeartbeatRequest Integer
  | Reconnect
  | InvalidSession Bool
  | Hello Integer
  | HeartbeatAck
  | ParseError T.Text
  deriving (Show, Eq, Read)

-- | Sent to gateway by our library
data GatewaySendableInternal
  = Heartbeat Integer
  | Identify Auth GatewayIntent (Int, Int)
  | Resume Auth T.Text Integer
  deriving (Show, Read, Eq, Ord)


-- | Gateway intents to subrscribe to
-- 
-- Details of which intent englobs what data is avalilable at
-- [the official Discord documentation](https://discord.com/developers/docs/topics/gateway#list-of-intents)
data GatewayIntent = GatewayIntent
  { gatewayIntentGuilds :: Bool
  , gatewayIntentMembers :: Bool
  , gatewayIntentBans :: Bool
  , gatewayIntentEmojis :: Bool
  , gatewayIntentIntegrations :: Bool
  , gatewayIntentWebhooks :: Bool
  , gatewayIntentInvites :: Bool
  , gatewayIntentVoiceStates :: Bool
  , gatewayIntentPresences :: Bool
  , gatewayIntentMessageChanges :: Bool
  , gatewayIntentMessageReactions :: Bool
  , gatewayIntentMessageTyping :: Bool
  , gatewayIntentDirectMessageChanges :: Bool
  , gatewayIntentDirectMessageReactions :: Bool
  , gatewayIntentDirectMessageTyping :: Bool
  , gatewayIntentMessageContent :: Bool
  } deriving (Show, Read, Eq, Ord)

instance Default GatewayIntent where
  def = GatewayIntent { gatewayIntentGuilds                 = True
                      , gatewayIntentMembers                = False -- false
                      , gatewayIntentBans                   = True
                      , gatewayIntentEmojis                 = True
                      , gatewayIntentIntegrations           = True
                      , gatewayIntentWebhooks               = True
                      , gatewayIntentInvites                = True
                      , gatewayIntentVoiceStates            = True
                      , gatewayIntentPresences              = False  -- false
                      , gatewayIntentMessageChanges         = True
                      , gatewayIntentMessageReactions       = True
                      , gatewayIntentMessageTyping          = True
                      , gatewayIntentDirectMessageChanges   = True
                      , gatewayIntentDirectMessageReactions = True
                      , gatewayIntentDirectMessageTyping    = True
                      , gatewayIntentMessageContent         = True
                      }

compileGatewayIntent :: GatewayIntent -> Int
compileGatewayIntent GatewayIntent{..} =
 sum $ [ if on then flag else 0
       | (flag, on) <- [ (     1, gatewayIntentGuilds)
                       , (2 ^  1, gatewayIntentMembers)
                       , (2 ^  2, gatewayIntentBans)
                       , (2 ^  3, gatewayIntentEmojis)
                       , (2 ^  4, gatewayIntentIntegrations)
                       , (2 ^  5, gatewayIntentWebhooks)
                       , (2 ^  6, gatewayIntentInvites)
                       , (2 ^  7, gatewayIntentVoiceStates)
                       , (2 ^  8, gatewayIntentPresences)
                       , (2 ^  9, gatewayIntentMessageChanges)
                       , (2 ^ 10, gatewayIntentMessageReactions)
                       , (2 ^ 11, gatewayIntentMessageTyping)
                       , (2 ^ 12, gatewayIntentDirectMessageChanges)
                       , (2 ^ 13, gatewayIntentDirectMessageReactions)
                       , (2 ^ 14, gatewayIntentDirectMessageTyping)
                       , (2 ^ 15, gatewayIntentMessageContent)
                       ]
       ]

-- | Sent to gateway by a user
data GatewaySendable
  = RequestGuildMembers RequestGuildMembersOpts
  | UpdateStatus UpdateStatusOpts
  | UpdateStatusVoice UpdateStatusVoiceOpts
  deriving (Show, Read, Eq, Ord)

-- | Options for `RequestGuildMembers`
data RequestGuildMembersOpts = RequestGuildMembersOpts
                             { requestGuildMembersOptsGuildId :: GuildId
                             , requestGuildMembersOptsNamesStartingWith :: T.Text
                             , requestGuildMembersOptsLimit :: Integer }
  deriving (Show, Read, Eq, Ord)

-- | Options for `UpdateStatusVoice`
data UpdateStatusVoiceOpts = UpdateStatusVoiceOpts
                           { updateStatusVoiceOptsGuildId :: GuildId
                           , updateStatusVoiceOptsChannelId :: Maybe ChannelId
                           , updateStatusVoiceOptsIsMuted :: Bool
                           , updateStatusVoiceOptsIsDeaf :: Bool
                           }
  deriving (Show, Read, Eq, Ord)

-- | Options for `UpdateStatus`
-- Presence Update - https://discord.com/developers/docs/topics/gateway-events#update-presence
data UpdateStatusOpts = UpdateStatusOpts
                      { updateStatusOptsSince :: Maybe UTCTime
                      , updateStatusOptsActivities :: [Activity]
                      , updateStatusOptsNewStatus :: UpdateStatusType
                      , updateStatusOptsAFK :: Bool
                      }
  deriving (Show, Read, Eq, Ord)

-- | Possible values for `updateStatusOptsNewStatus`
data UpdateStatusType = UpdateStatusOnline
                      | UpdateStatusDoNotDisturb
                      | UpdateStatusAwayFromKeyboard
                      | UpdateStatusInvisibleOffline
                      | UpdateStatusOffline
  deriving (Show, Read, Eq, Ord, Enum)


-- | Converts an UpdateStatusType to a textual representation
statusString :: UpdateStatusType -> T.Text
statusString s = case s of
  UpdateStatusOnline -> "online"
  UpdateStatusDoNotDisturb -> "dnd"
  UpdateStatusAwayFromKeyboard -> "idle"
  UpdateStatusInvisibleOffline -> "invisible"
  UpdateStatusOffline -> "offline"

instance FromJSON GatewayReceivable where
  parseJSON = withObject "payload" $ \o -> do
    op <- o .: "op" :: Parser Int
    case op of
      0  -> do etype <- o .: "t"
               ejson <- o .: "d"
               case ejson of
                 Object hm -> Dispatch <$> eventParse etype hm <*> o .: "s"
                 _other -> Dispatch (InternalUnknownEvent "Dispatch payload wasn't an object" o)
                                  <$> o .: "s"
      1  -> HeartbeatRequest . fromMaybe 0 . readMaybe <$> o .: "d"
      7  -> pure Reconnect
      9  -> InvalidSession <$> o .: "d"
      10 -> do od <- o .: "d"
               int <- od .: "heartbeat_interval"
               pure (Hello int)
      11 -> pure HeartbeatAck
      _  -> fail ("Unknown Receivable payload ID:" <> show op)

-- instance FromJSON GatewaySendable where
--   parseJSON = withObject "payload" $ \o -> do
--     op <- o .: "op" :: Parser Int
--     case op of
--       1  -> Heartbeat . fromMaybe 0 . readMaybe <$> o .: "d"
--       2  -> do od <- o .: "d"
--                tok <- od .: "token"
--                compress <- od .:? "compress" .!= False
--
--       _  -> fail ("Unknown Sendable payload ID:" <> show op)

instance ToJSON GatewaySendableInternal where
  toJSON (Heartbeat i) = object [ "op" .= (1 :: Int), "d" .= if i <= 0 then "null" else show i ]
  toJSON (Identify token intent shard) = object [
      "op" .= (2 :: Int)
    , "d"  .= object [
        "token" .= authToken token
      , "intents" .= compileGatewayIntent intent
      , "properties" .= object [
          "$os"                .= os
        , "$browser"           .= ("discord-haskell" :: T.Text)
        , "$device"            .= ("discord-haskell" :: T.Text)
        , "$referrer"          .= (""                :: T.Text)
        , "$referring_domain"  .= (""                :: T.Text)
        ]
      , "compress" .= False
      , "large_threshold" .= (50 :: Int) -- stop sending offline members over 50
      , "shard" .= shard
      ]
    ]
  toJSON (Resume token session seqId) = object [
      "op" .= (6 :: Int)
    , "d"  .= object [
        "token"      .= authToken token
      , "session_id" .= session
      , "seq"        .= seqId
      ]
    ]

instance ToJSON GatewaySendable where
  toJSON (UpdateStatus (UpdateStatusOpts since activities status afk)) = object [
      "op" .= (3 :: Int)
    , "d"  .= object [
        "since" .= (since <&> \s -> 1000 * utcTimeToPOSIXSeconds s) -- takes UTCTime and returns unix time (in milliseconds)
      , "afk" .= afk
      , "status" .= statusString status
      , "activities" .= activities
      ]
    ]
  toJSON (UpdateStatusVoice (UpdateStatusVoiceOpts guild channel mute deaf)) =
    object [
      "op" .= (4 :: Int)
    , "d"  .= object [
        "guild_id"   .= guild
      , "channel_id" .= channel
      , "self_mute"  .= mute
      , "self_deaf"  .= deaf
      ]
    ]
  toJSON (RequestGuildMembers (RequestGuildMembersOpts guild query limit)) =
    object [
      "op" .= (8 :: Int)
    , "d"  .= object [
        "guild_id" .= guild
      , "query"    .= query
      , "limit"    .= limit
      ]
    ]
