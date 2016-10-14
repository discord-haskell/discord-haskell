{-# LANGUAGE OverloadedStrings #-}
module Network.Discord.Gateway where 
  import Data.Maybe
  import System.Info
  import qualified Data.ByteString.Lazy as BS

  import Data.Aeson
  import Data.Aeson.Types
  import Network.WebSockets

  import Network.Discord.Types

  type Auth = String
  data Payload = Dispatch
    Object
    Integer
    String
               | Heartbeat
    Integer
               | Identify
    Auth
    Bool
    Integer
   (Int, Int)
               | StatusUpdate
   (Maybe Integer)
   (Maybe String)
               | VoiceStatusUpdate
    Snowflake
   (Maybe Snowflake)
    Bool
    Bool
               | VoiceServerPing
               | Resume
    String
    String
    Integer
               | Reconnect
               | RequestGuildMembers
    Snowflake
    String
    Integer
               | InvalidSession
               | Hello
    Int
               | HeartbeatAck
               | ParseError String
    deriving Show
  instance Enum Payload where -- Note: Payload is an Enum only because I'm dumb and it's the only way I can think of to get JSON to play nice
    fromEnum VoiceServerPing = 5
    fromEnum Reconnect       = 7
    fromEnum InvalidSession  = 9
    fromEnum HeartbeatAck    = 11
    toEnum 5  = VoiceServerPing
    toEnum 7  = Reconnect
    toEnum 9  = InvalidSession
    toEnum 11 = HeartbeatAck

  instance FromJSON Payload where
    parseJSON = withObject "payload" $ \o -> do
      op <- o .: "op" :: Parser Int
      case op of
        0  -> Dispatch <$> o .: "d" <*> o .: "s" <*> o .: "t"
        1  -> Heartbeat <$> o .: "d"
        10 -> (\od -> Hello <$> od .: "heartbeat_interval") =<< o .: "d"
        _  -> toEnum <$> o .: "op"

  instance ToJSON Payload where
    toJSON (Heartbeat i) = object [ "op" .= (1 :: Int), "d" .= i ]
    toJSON (Identify token compress large shard) = object [
        "op" .= (2 :: Int)
      , "d"  .= object [
          "token" .= token 
        , "properties" .= object [
            "$os"                .= os
          , "$browser"           .= ("discord.hs" :: String)
          , "$device"            .= ("discord.hs" :: String)
          , "$referrer"          .= (""           :: String)
          , "$referring_domain"  .= (""           :: String)
          ]
        , "compress" .= compress
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
    toJSON (Resume token session seq) = object [
        "op" .= (6 :: Int)
      , "d"  .= object [
          "token"      .= token
        , "session_id" .= session
        , "seq"        .= seq
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
  instance WebSocketsData Payload where
    fromLazyByteString bs = case eitherDecode bs of
        Right payload -> payload
        Left  reason  -> ParseError reason
    toLazyByteString   = encode

