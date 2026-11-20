{-# LANGUAGE OverloadedStrings #-}

-- | Data structures pertaining to Discord soundboard sounds
module Discord.Internal.Types.Soundboard where

import Data.Aeson
import qualified Data.Text as T

import Discord.Internal.Types.Prelude
import Discord.Internal.Types.User (User)

-- | A sound that can be played in a voice channel. Detailed information can be found at <https://discord.com/developers/docs/resources/soundboard#soundboard-sound-object>.
data SoundboardSound = SoundboardSound
  { soundboardSoundName :: T.Text -- ^ Name of the sound
  , soundboardSoundId :: SoundboardSoundId -- ^ Id of the sound
  , soundboardSoundVolume :: Double -- ^ Volume of the sound, from 0 to 1
  , soundboardSoundEmojiId :: Maybe EmojiId -- ^ Id of the sound's custom emoji
  , soundboardSoundEmojiName :: Maybe T.Text -- ^ Unicode character of the sound's standard emoji
  , soundboardSoundGuildId :: Maybe GuildId -- ^ Guild the sound belongs to, or 'Nothing' for a default sound
  , soundboardSoundAvailable :: Bool -- ^ Whether the sound can be used. May be 'False' if the guild lost Server Boosts
  , soundboardSoundUser :: Maybe User -- ^ User who created the sound, or 'Nothing' where Discord does not send it, as for a default sound
  } deriving (Show, Read, Eq, Ord)

instance FromJSON SoundboardSound where
  parseJSON = withObject "SoundboardSound" $ \o ->
    SoundboardSound <$> o .:  "name"
                    <*> o .:  "sound_id"
                    <*> o .:  "volume"
                    <*> o .:? "emoji_id"
                    <*> o .:? "emoji_name"
                    <*> o .:? "guild_id"
                    <*> o .:  "available"
                    <*> o .:? "user"
