{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Discord.Internal.Types.Emoji where

import Data.Aeson
import Data.Data
import Data.Functor ((<&>))
import Data.Text as T
import Discord.Internal.Types.Prelude
import Discord.Internal.Types.User

-- | Represents an emoticon (emoji)
data Emoji = Emoji
  { -- | The emoji id
    emojiId :: Maybe EmojiId,
    -- | The emoji name
    emojiName :: T.Text,
    -- | Roles the emoji is active for
    emojiRoles :: Maybe [RoleId],
    -- | User that created this emoji
    emojiUser :: Maybe User,
    -- | Whether this emoji is managed
    emojiManaged :: Maybe Bool,
    -- | Whether this emoji is animated
    emojiAnimated :: Maybe Bool
  }
  deriving (Show, Read, Eq, Ord)

-- | Make an emoji with only a name
mkEmoji :: T.Text -> Emoji
mkEmoji t = Emoji Nothing t Nothing Nothing Nothing Nothing

instance FromJSON Emoji where
  parseJSON = withObject "Emoji" $ \o ->
    Emoji <$> o .:? "id"
      <*> o .: "name"
      <*> o .:? "roles"
      <*> o .:? "user"
      <*> o .:? "managed"
      <*> o .:? "animated"

instance ToJSON Emoji where
  toJSON Emoji {..} =
    objectFromMaybes
            [ "id" .=? emojiId,
              "name" .== emojiName,
              "roles" .=? emojiRoles,
              "user" .=? emojiUser,
              "managed" .=? emojiManaged,
              "animated" .=? emojiAnimated
            ]

-- | Represents a pack of standard stickers.
data StickerPack = StickerPack
  { -- | The id of the sticker pack
    stickerPackId :: Snowflake,
    -- | The stickers in the pack
    stickerPackStickers :: [Sticker],
    -- | The name of the sticker pack
    stickerPackName :: T.Text,
    -- | ID of the pack's SKU
    stickerPackSKUId :: Snowflake,
    -- | If of the sticker which is shown as the pack's icon
    stickerPackCoverStickerId :: Maybe StickerId,
    -- | The description of the sticker pack
    stickerPackDescription :: T.Text,
    -- | Id of the sticker pack's banner image
    stickerPackBannerAssetId :: Maybe Snowflake
  }
  deriving (Show, Read, Eq, Ord)

instance FromJSON StickerPack where
  parseJSON = withObject "StickerPack" $ \o ->
    StickerPack <$> o .: "id"
      <*> o .: "stickers"
      <*> o .: "name"
      <*> o .: "sku_id"
      <*> o .:? "cover_sticker_id"
      <*> o .: "description"
      <*> o .:? "banner_asset_id"

-- | A full sticker object
data Sticker = Sticker
  { -- | The sticker's id.
    stickerId :: StickerId,
    -- | For standard stickers, the id of the pack.
    stickerStickerPackId :: Maybe Snowflake,
    -- | The sticker's name.
    stickerName :: T.Text,
    -- | The sticker's description.
    stickerDescription :: Maybe T.Text,
    -- | Autocomplete/suggestion tags for the sticker (max 200 characters total).
    stickerTags :: [T.Text],
    -- | Whether the sticker is standard or guild type.
    stickerIsStandardType :: Bool,
    -- | The sticker's format type.
    stickerFormatType :: StickerFormatType,
    -- | Whether this guild sticker can be used.
    stickerAvailable :: Maybe Bool,
    -- | What guild owns this sticker.
    stickerGuildId :: Maybe GuildId,
    -- | What user uploaded the guild sticker.
    stickerUser :: Maybe User,
    -- | A standard sticker's sort order in its pack.
    stickerSortValue :: Maybe Integer
  }
  deriving (Show, Read, Eq, Ord)

instance FromJSON Sticker where
  parseJSON = withObject "Sticker" $ \o ->
    Sticker <$> o .: "id"
      <*> o .:? "pack_id"
      <*> o .:  "name"
      <*> o .:? "description"
      <*> ((o .: "tags") <&> T.splitOn "\n")
      <*> ((o .: "type") <&> (== (1 :: Int)))
      <*> o .: "format_type"
      <*> o .:? "available"
      <*> o .:? "guild_id"
      <*> o .:? "user"
      <*> o .:? "sort_value"

-- | A simplified sticker object.
data StickerItem = StickerItem
  { -- | The sticker's id.
    stickerItemId :: StickerId,
    -- | The sticker's name.
    stickerItemName :: T.Text,
    -- | The sticker's format type.
    stickerItemFormatType :: StickerFormatType
  }
  deriving (Show, Read, Eq, Ord)

instance FromJSON StickerItem where
  parseJSON = withObject "StickerItem" $ \o ->
    StickerItem <$> o .: "id"
      <*> o .: "name"
      <*> o .: "format_type"

instance ToJSON StickerItem where
  toJSON StickerItem {..} =
    object
      [ ("id", toJSON stickerItemId),
        ("name", toJSON stickerItemName),
        ("format_type", toJSON stickerItemFormatType)
      ]

-- | The format of a sticker
data StickerFormatType
  = StickerFormatTypePNG
  | StickerFormatTypeAPNG
  | StickerFormatTypeLOTTIE
  deriving (Show, Read, Eq, Ord, Data)

instance InternalDiscordEnum StickerFormatType where
  discordTypeStartValue = StickerFormatTypePNG
  fromDiscordType StickerFormatTypePNG = 1
  fromDiscordType StickerFormatTypeAPNG = 2
  fromDiscordType StickerFormatTypeLOTTIE = 3

instance ToJSON StickerFormatType where
  toJSON = toJSON . fromDiscordType

instance FromJSON StickerFormatType where
  parseJSON = discordTypeParseJSON "StickerFormatType"
