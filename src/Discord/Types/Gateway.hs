{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

-- | Data structures needed for interfacing with the Websocket
--   Gateway
module Discord.Types.Gateway where

import Control.Monad (mzero)
import System.Info

import qualified Data.Text.Encoding as TE
import qualified Data.Text as T
import qualified Data.ByteString as Q
import Data.Monoid ((<>))
import Data.Aeson
import Data.Aeson.Types
import Data.Maybe (fromMaybe)
import Text.Read (readMaybe)

import Discord.Types.Prelude
import Discord.Types.Events

-- |Represents data sent and received with Discord servers
data Payload
  = Dispatch Event Integer
  | Heartbeat Integer
  | Identify Auth Bool Integer (Int, Int)
  | StatusUpdate (Maybe Integer) (Maybe String)
  | VoiceStatusUpdate Snowflake (Maybe Snowflake) Bool Bool
  | Resume T.Text String Integer
  | Reconnect
  | RequestGuildMembers Snowflake String Integer
  | InvalidSession Bool
  | Hello Int
  | HeartbeatAck
  | ParseError String
  deriving Show

instance FromJSON Payload where
  parseJSON = withObject "payload" $ \o -> do
    op <- o .: "op" :: Parser Int
    case op of
      0  -> do etype <- o .: "t"
               ejson <- o .: "d"
               case ejson of
                 Object hm -> Dispatch <$> eventParse etype hm <*> o .: "s"
                 _other -> Dispatch (UnknownEvent ("Dispatch payload wasn't an object") o)
                                  <$> o .: "s"
      1  -> Heartbeat . fromMaybe 0 . readMaybe <$> o .: "d"
      7  -> pure Reconnect
      9  -> InvalidSession <$> o .: "d"
      10 -> do od <- o .: "d"
               int <- od .: "heartbeat_interval"
               pure (Hello int)
      11 -> pure HeartbeatAck
      _  -> fail ("Unknown payload ID:" <> show op)

instance ToJSON Payload where
  toJSON (Heartbeat i) = object [ "op" .= (1 :: Int), "d" .= if i <= 0 then "null" else show i ]
  toJSON (Identify token compress large shard) = object [
      "op" .= (2 :: Int)
    , "d"  .= object [
        "token" .= authToken token
      , "properties" .= object [
          "$os"                .= os
        , "$browser"           .= ("discord-haskell" :: String)
        , "$device"            .= ("discord-haskell" :: String)
        , "$referrer"          .= (""                :: String)
        , "$referring_domain"  .= (""                :: String)
        ]
      , "compress" .= compress
      , "large_threshold" .= large
      , "shard" .= shard
      ]
    ]
  toJSON (StatusUpdate idle game) = object [
      "op" .= (3 :: Int)
    , "d"  .= object [
        "idle_since" .= idle
      , "game"       .= object [
          "name" .= game
        ]
      ]
    ]
  toJSON (VoiceStatusUpdate guild channel mute deaf) = object [
      "op" .= (4 :: Int)
    , "d"  .= object [
        "guild_id"   .= guild
      , "channel_id" .= channel
      , "self_mute"  .= mute
      , "self_deaf"  .= deaf
      ]
    ]
  toJSON (Resume token session seqId) = object [
      "op" .= (6 :: Int)
    , "d"  .= object [
        "token"      .= token
      , "session_id" .= session
      , "seq"        .= seqId
      ]
    ]
  toJSON (RequestGuildMembers guild query limit) = object [
      "op" .= (8 :: Int)
    , "d"  .= object [
        "guild_id" .= guild
      , "query"    .= query
      , "limit"    .= limit
      ]
    ]
  toJSON _ = object []
