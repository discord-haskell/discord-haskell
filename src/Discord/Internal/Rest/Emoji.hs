{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Provides actions for Channel API interactions
module Discord.Internal.Rest.Emoji
  ( EmojiRequest (..),
    ModifyGuildEmojiOpts (..),
    parseEmojiImage,
    parseStickerImage,
    StickerRequest (..),
    CreateGuildStickerOpts (..),
    EditGuildStickerOpts (..)
  )
where

import Data.Aeson
import qualified Data.ByteString as B
import qualified Data.ByteString.Base64 as B64
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Discord.Internal.Rest.Prelude
import Discord.Internal.Types
import Network.HTTP.Req ((/:), (/~))
import qualified Network.HTTP.Req as R

instance Request (EmojiRequest a) where
  majorRoute = emojiMajorRoute
  jsonRequest = emojiJsonRequest

-- | Data constructor for requests. See <https://discord.com/developers/docs/resources/ API>
data EmojiRequest a where
  -- | List of emoji objects for the given guild. Requires MANAGE_EMOJIS permission.
  ListGuildEmojis :: GuildId -> EmojiRequest [Emoji]
  -- | Emoji object for the given guild and emoji ID
  GetGuildEmoji :: GuildId -> EmojiId -> EmojiRequest Emoji
  -- | Create a new guild emoji (static&animated). Requires MANAGE_EMOJIS permission.
  CreateGuildEmoji :: GuildId -> T.Text -> Base64Image Emoji -> EmojiRequest Emoji
  -- | Requires MANAGE_EMOJIS permission
  ModifyGuildEmoji :: GuildId -> EmojiId -> ModifyGuildEmojiOpts -> EmojiRequest Emoji
  -- | Requires MANAGE_EMOJIS permission
  DeleteGuildEmoji :: GuildId -> EmojiId -> EmojiRequest ()

data ModifyGuildEmojiOpts = ModifyGuildEmojiOpts
  { modifyGuildEmojiName :: T.Text,
    modifyGuildEmojiRoles :: [RoleId]
  }
  deriving (Show, Read, Eq, Ord)

instance ToJSON ModifyGuildEmojiOpts where
  toJSON (ModifyGuildEmojiOpts name roles) =
    object ["name" .= name, "roles" .= roles]


-- | @parseEmojiImage bs@ will attempt to convert the given image bytestring @bs@
-- to the base64 format expected by the Discord API. It may return Left with an
-- error reason if either the bytestring is too large, or if the image format
-- could not be predetermined from the opening few bytes. This function does
-- /not/ validate the rest of the image, nor check that its dimensions are
-- 128x128 as required by Discord. This is up to the library user to check.
--
-- This function accepts all file types accepted by 'getMimeType'.
parseEmojiImage :: B.ByteString -> Either T.Text (Base64Image Emoji)
parseEmojiImage bs
  | B.length bs > 256000        = Left "Cannot create emoji - File is larger than 256kb"
  | Just mime <- getMimeType bs = Right (Base64Image mime (TE.decodeUtf8 (B64.encode bs)))
  | otherwise                   = Left "Unsupported image format provided"

emojiMajorRoute :: EmojiRequest a -> String
emojiMajorRoute c = case c of
  (ListGuildEmojis g) -> "emoji " <> show g
  (GetGuildEmoji g _) -> "emoji " <> show g
  (CreateGuildEmoji g _ _) -> "emoji " <> show g
  (ModifyGuildEmoji g _ _) -> "emoji " <> show g
  (DeleteGuildEmoji g _) -> "emoji " <> show g

guilds :: R.Url 'R.Https
guilds = baseUrl /: "guilds"

emojiJsonRequest :: EmojiRequest r -> JsonRequest
emojiJsonRequest c = case c of
  (ListGuildEmojis g) -> Get (guilds /~ g /: "emojis") mempty
  (GetGuildEmoji g e) -> Get (guilds /~ g /: "emojis" /~ e) mempty
  (CreateGuildEmoji g name b64im) ->
    Post
      (guilds /~ g /: "emojis")
      ( pure
          ( R.ReqBodyJson
              ( object
                  [ "name" .= name,
                    "image" .= b64im
                    -- todo , "roles" .= ...
                  ]
              )
          )
      )
      mempty
  (ModifyGuildEmoji g e o) ->
    Patch
      (guilds /~ g /: "emojis" /~ e)
      (pure (R.ReqBodyJson o))
      mempty
  (DeleteGuildEmoji g e) -> Delete (guilds /~ g /: "emojis" /~ e) mempty

-- | @parseStickerImage bs@ accepts PNG, APNG, or Lottie JSON bytestring @bs@ and
-- will attempt to convert it to the base64 format expected by the Discord API.
-- It may return Left with an error reason if the image format is unexpected.
-- This function does /not/ validate the contents of the image, this is up to
-- the library user to check.
parseStickerImage :: B.ByteString -> Either T.Text (Base64Image Sticker)
parseStickerImage bs
  | B.length bs > 512000
  = Left "Cannot create sticker - File is larger than 512kb"
  | Just "image/png" <- getMimeType bs
  = Right (Base64Image "image/png" (TE.decodeUtf8 (B64.encode bs)))
  | not (B.null bs) && B.head bs == 0x7b -- '{'
  = Right (Base64Image "application/json" (TE.decodeUtf8 (B64.encode bs)))
  | otherwise
  = Left "Unsupported image format provided"

-- | Options for `CreateGuildSticker`
data CreateGuildStickerOpts = CreateGuildStickerOpts
  { guildStickerName :: T.Text,
    guildStickerDescription :: T.Text,
    guildStickerTags :: [T.Text],
    guildStickerFile :: Base64Image Sticker
  }
  deriving (Show, Read, Eq, Ord)

instance ToJSON CreateGuildStickerOpts where
  toJSON (CreateGuildStickerOpts name desc tags b64im) =
    object
      [ ("name", toJSON name),
        ("description", toJSON desc),
        ("tags", toJSON $ T.intercalate "," tags),
        ("file", toJSON b64im)
      ]

-- | Options for `ModifyGuildSticker`
data EditGuildStickerOpts = EditGuildStickerOpts
  { editGuildStickerName :: Maybe T.Text,
    editGuildStickerDescription :: Maybe T.Text,
    editGuildStickerTags :: Maybe [T.Text]
  }
  deriving (Show, Read, Eq, Ord)

instance ToJSON EditGuildStickerOpts where
  toJSON EditGuildStickerOpts {..} =
    objectFromMaybes
      [ "name" .=? editGuildStickerName,
        "description" .=? editGuildStickerDescription,
        "tags" .=? fmap (T.intercalate ",") editGuildStickerTags
      ]

instance Request (StickerRequest a) where
  majorRoute = stickerMajorRoute
  jsonRequest = stickerJsonRequest

-- | Data constructor for requests. See <https://discord.com/developers/docs/resources/ API>
--
-- Be warned that these are untested due to not having a spare server with 
-- boosts. Functionality is at your own risk.
data StickerRequest a where
  -- | Returns a sticker object for the given sticker ID.
  GetSticker :: StickerId -> StickerRequest Sticker
  -- | Returns the list of sticker packs available to Nitro subscribers.
  ListNitroStickerPacks :: StickerRequest [StickerPack]
  -- | Returns an array of sticker objects for the given guild.
  ListGuildStickers :: GuildId -> StickerRequest [Sticker]
  -- | Returns a sticker object for the given guild and sticker ID.
  GetGuildSticker :: GuildId -> StickerId -> StickerRequest Sticker
  -- | Create a new sticker for the guild.
  CreateGuildSticker :: GuildId -> CreateGuildStickerOpts -> StickerRequest Sticker
  -- | Modify a sticker for a guild.
  ModifyGuildSticker :: GuildId -> StickerId -> EditGuildStickerOpts -> StickerRequest Sticker
  -- | Delete a guild sticker
  DeleteGuildSticker :: GuildId -> StickerId -> StickerRequest ()

stickerMajorRoute :: StickerRequest a -> String
stickerMajorRoute = \case
  GetSticker gid -> "sticker " <> show gid
  ListNitroStickerPacks -> "sticker"
  ListGuildStickers gid -> "sticker " <> show gid
  GetGuildSticker gid _ -> "sticker " <> show gid
  CreateGuildSticker gid _ -> "sticker " <> show gid
  ModifyGuildSticker gid _ _ -> "sticker " <> show gid
  DeleteGuildSticker gid _ -> "sticker " <> show gid

stickerJsonRequest :: StickerRequest a -> JsonRequest
stickerJsonRequest = \case
  GetSticker gid -> Get (baseUrl /: "stickers" /~ gid) mempty
  ListNitroStickerPacks -> Get (baseUrl /: "sticker-packs") mempty
  ListGuildStickers gid -> Get (stickersGuild gid) mempty
  GetGuildSticker gid sid -> Get (stickersGuild gid /~ sid) mempty
  CreateGuildSticker gid cgso -> Post (stickersGuild gid) (pure $ R.ReqBodyJson $ toJSON cgso) mempty
  ModifyGuildSticker gid sid egso -> Patch (stickersGuild gid /~ sid) (pure $ R.ReqBodyJson egso) mempty
  DeleteGuildSticker gid sid -> Delete (stickersGuild gid /~ sid) mempty
  where
    stickersGuild gid = baseUrl /: "guilds" /~ gid /: "stickers"
