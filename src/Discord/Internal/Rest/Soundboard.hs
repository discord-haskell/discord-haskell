{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Provides actions for Soundboard API interactions
module Discord.Internal.Rest.Soundboard
  ( SoundboardRequest(..)
  , SendSoundboardSoundOpts(..)
  , CreateGuildSoundboardSoundOpts(..)
  , ModifyGuildSoundboardSoundOpts(..)
  , GuildSoundboardSounds(..)
  , parseSoundboardSound
  ) where

import Data.Aeson
import Data.Bits ((.&.))
import qualified Data.ByteString as B
import qualified Data.ByteString.Base64 as B64
import Data.Default (Default(..))
import qualified Data.Text as T
import Network.HTTP.Req ((/:), (/~))
import qualified Network.HTTP.Req as R

import Discord.Internal.Rest.Prelude
import Discord.Internal.Types

instance Request (SoundboardRequest a) where
  majorRoute = soundboardMajorRoute
  jsonRequest = soundboardJsonRequest

-- | Data constructor for requests. See <https://discord.com/developers/docs/resources/soundboard API>
data SoundboardRequest a where
  -- | Plays a 'SoundboardSound' in a voice channel the current user is connected
  --   to. Requires the 'SPEAK' and USE_SOUNDBOARD permissions, and
  --   USE_EXTERNAL_SOUNDS for a sound from another guild. Fires a Voice
  --   Channel Effect Send 'Event'.
  SendSoundboardSound :: ChannelId -> SendSoundboardSoundOpts -> SoundboardRequest ()
  -- | Returns the 'SoundboardSound's available to all users.
  ListDefaultSoundboardSounds :: SoundboardRequest [SoundboardSound]
  -- | Returns the 'SoundboardSound's of a guild. The creator of each sound is
  --   only included with the CREATE_GUILD_EXPRESSIONS or
  --   MANAGE_GUILD_EXPRESSIONS permission.
  ListGuildSoundboardSounds :: GuildId -> SoundboardRequest GuildSoundboardSounds
  -- | Returns a 'SoundboardSound' of a guild. The creator of the sound is only
  --   included with the CREATE_GUILD_EXPRESSIONS or
  --   MANAGE_GUILD_EXPRESSIONS permission.
  GetGuildSoundboardSound :: GuildId -> SoundboardSoundId -> SoundboardRequest SoundboardSound
  -- | Creates a 'SoundboardSound' in a guild. Requires the
  --   CREATE_GUILD_EXPRESSIONS permission. Fires a Guild Soundboard Sound
  --   Create 'Event'.
  CreateGuildSoundboardSound :: GuildId -> CreateGuildSoundboardSoundOpts -> SoundboardRequest SoundboardSound
  -- | Modifies a 'SoundboardSound' of a guild. Requires the
  --   CREATE_GUILD_EXPRESSIONS permission for a sound the current user
  --   created, and MANAGE_GUILD_EXPRESSIONS for any other. Fires a Guild
  --   Soundboard Sound Update 'Event'.
  ModifyGuildSoundboardSound :: GuildId -> SoundboardSoundId -> ModifyGuildSoundboardSoundOpts -> SoundboardRequest SoundboardSound
  -- | Deletes a 'SoundboardSound' of a guild. Requires the
  --   CREATE_GUILD_EXPRESSIONS permission for a sound the current user
  --   created, and MANAGE_GUILD_EXPRESSIONS for any other. Fires a Guild
  --   Soundboard Sound Delete 'Event'.
  DeleteGuildSoundboardSound :: GuildId -> SoundboardSoundId -> SoundboardRequest ()

-- | Options for `SendSoundboardSound`
data SendSoundboardSoundOpts = SendSoundboardSoundOpts
  { sendSoundboardSoundOptsSoundId :: SoundboardSoundId -- ^ The sound to play
  , sendSoundboardSoundOptsSourceGuildId :: Maybe GuildId -- ^ The guild the sound is from. Required for a sound from another guild
  } deriving (Show, Read, Eq, Ord)

instance ToJSON SendSoundboardSoundOpts where
  toJSON SendSoundboardSoundOpts{..} = objectFromMaybes
                       [ "sound_id" .== sendSoundboardSoundOptsSoundId
                       , "source_guild_id" .=? sendSoundboardSoundOptsSourceGuildId ]

-- | Result of `ListGuildSoundboardSounds`
newtype GuildSoundboardSounds = GuildSoundboardSounds
  { guildSoundboardSoundsItems :: [SoundboardSound]
  } deriving (Show, Read, Eq, Ord)

instance FromJSON GuildSoundboardSounds where
  parseJSON = withObject "GuildSoundboardSounds" $ \o ->
    GuildSoundboardSounds <$> o .: "items"

-- | Options for `CreateGuildSoundboardSound`
data CreateGuildSoundboardSoundOpts = CreateGuildSoundboardSoundOpts
  { createGuildSoundboardSoundOptsName :: T.Text -- ^ Name of the sound, from 2 to 32 characters
  , createGuildSoundboardSoundOptsSound :: Base64Image SoundboardSound -- ^ The sound file, made with 'parseSoundboardSound'
  , createGuildSoundboardSoundOptsVolume :: Maybe Double -- ^ Volume of the sound, from 0 to 1. Discord defaults it to 1
  , createGuildSoundboardSoundOptsEmojiId :: Maybe EmojiId -- ^ Id of the sound's custom emoji
  , createGuildSoundboardSoundOptsEmojiName :: Maybe T.Text -- ^ Unicode character of the sound's standard emoji
  } deriving (Show, Read, Eq, Ord)

instance ToJSON CreateGuildSoundboardSoundOpts where
  toJSON CreateGuildSoundboardSoundOpts{..} = objectFromMaybes
                       [ "name" .== createGuildSoundboardSoundOptsName
                       , "sound" .== createGuildSoundboardSoundOptsSound
                       , "volume" .=? createGuildSoundboardSoundOptsVolume
                       , "emoji_id" .=? createGuildSoundboardSoundOptsEmojiId
                       , "emoji_name" .=? createGuildSoundboardSoundOptsEmojiName ]

-- | Options for `ModifyGuildSoundboardSound`
data ModifyGuildSoundboardSoundOpts = ModifyGuildSoundboardSoundOpts
  { modifyGuildSoundboardSoundOptsName :: Maybe T.Text -- ^ Name of the sound, from 2 to 32 characters
  , modifyGuildSoundboardSoundOptsVolume :: Maybe Double -- ^ Volume of the sound, from 0 to 1
  , modifyGuildSoundboardSoundOptsEmojiId :: Maybe EmojiId -- ^ Id of the sound's custom emoji
  , modifyGuildSoundboardSoundOptsEmojiName :: Maybe T.Text -- ^ Unicode character of the sound's standard emoji
  } deriving (Show, Read, Eq, Ord)

instance Default ModifyGuildSoundboardSoundOpts where
  def = ModifyGuildSoundboardSoundOpts Nothing Nothing Nothing Nothing

instance ToJSON ModifyGuildSoundboardSoundOpts where
  toJSON ModifyGuildSoundboardSoundOpts{..} = objectFromMaybes
                       [ "name" .=? modifyGuildSoundboardSoundOptsName
                       , "volume" .=? modifyGuildSoundboardSoundOptsVolume
                       , "emoji_id" .=? modifyGuildSoundboardSoundOptsEmojiId
                       , "emoji_name" .=? modifyGuildSoundboardSoundOptsEmojiName ]

-- | @parseSoundboardSound bs@ will attempt to convert the given MP3 or Ogg
-- bytestring @bs@ to the base64 format expected by the Discord API. It may
-- return Left with an error reason if the bytestring is too large, or if the
-- audio format could not be predetermined from the opening few bytes. This
-- function does /not/ validate the rest of the audio, nor check that it is
-- shorter than the 5.2 seconds Discord allows. This is up to the library user
-- to check.
parseSoundboardSound :: B.ByteString -> Either T.Text (Base64Image SoundboardSound)
parseSoundboardSound bs
  | B.length bs > 512000 = Left "Cannot create sound - File is larger than 512kb"
  | B.take 4 bs == "OggS" = Right (Base64Image "audio/ogg" (B64.encode bs))
  | isMp3 = Right (Base64Image "audio/mpeg" (B64.encode bs))
  | otherwise = Left "Unsupported audio format provided"
  where
    -- An ID3 tag, or the sync bits of an MPEG audio frame
    isMp3 = B.take 3 bs == "ID3"
      || (B.length bs >= 2 && B.index bs 0 == 0xff && B.index bs 1 .&. 0xe0 == 0xe0)

soundboardMajorRoute :: SoundboardRequest a -> String
soundboardMajorRoute c = case c of
  (SendSoundboardSound c' _) -> "soundboard " <> show c'
  (ListDefaultSoundboardSounds) -> "soundboard"
  (ListGuildSoundboardSounds g) -> "soundboard " <> show g
  (GetGuildSoundboardSound g _) -> "soundboard " <> show g
  (CreateGuildSoundboardSound g _) -> "soundboard " <> show g
  (ModifyGuildSoundboardSound g _ _) -> "soundboard " <> show g
  (DeleteGuildSoundboardSound g _) -> "soundboard " <> show g

soundboardJsonRequest :: SoundboardRequest r -> JsonRequest
soundboardJsonRequest c = case c of
  (SendSoundboardSound c' o) ->
    Post (baseUrl /: "channels" /~ c' /: "send-soundboard-sound") (pure $ R.ReqBodyJson $ toJSON o) mempty
  (ListDefaultSoundboardSounds) -> Get (baseUrl /: "soundboard-default-sounds") mempty
  (ListGuildSoundboardSounds g) -> Get (sounds g) mempty
  (GetGuildSoundboardSound g s) -> Get (sounds g /~ s) mempty
  (CreateGuildSoundboardSound g o) ->
    Post (sounds g) (pure $ R.ReqBodyJson $ toJSON o) mempty
  (ModifyGuildSoundboardSound g s o) ->
    Patch (sounds g /~ s) (pure $ R.ReqBodyJson $ toJSON o) mempty
  (DeleteGuildSoundboardSound g s) -> Delete (sounds g /~ s) mempty
  where
    sounds g = baseUrl /: "guilds" /~ g /: "soundboard-sounds"
