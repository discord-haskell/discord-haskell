{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Provides actions for Channel API interactions
module Discord.Rest.Emoji
  ( EmojiRequest(..)
  , ModifyGuildEmojiOpts(..)
  , EmojiImage
  , parseEmojiImage
  ) where

import Data.Aeson
import Data.Monoid (mempty, (<>))
import Codec.Picture
import Network.HTTP.Req ((/:))
import qualified Network.HTTP.Req as R
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as Q
import qualified Data.ByteString.Base64 as B64

import Discord.Rest.Prelude
import Discord.Types

instance Request (EmojiRequest a) where
  majorRoute = emojiMajorRoute
  jsonRequest = emojiJsonRequest


-- | Data constructor for requests. See <https://discordapp.com/developers/docs/resources/ API>
data EmojiRequest a where
  -- | List of emoji objects for the given guild. Requires MANAGE_EMOJIS permission.
  ListGuildEmojis :: Snowflake -> EmojiRequest [Emoji]
  -- | Emoji object for the given guild and emoji ID
  GetGuildEmoji :: Snowflake -> Snowflake -> EmojiRequest Emoji
  -- | Create a new guild emoji. Requires MANAGE_EMOJIS permission.
  CreateGuildEmojiPng :: Snowflake -> T.Text -> EmojiImage -> EmojiRequest Emoji
  -- | Requires MANAGE_EMOJIS permission
  ModifyGuildEmoji :: Snowflake -> Snowflake -> ModifyGuildEmojiOpts -> EmojiRequest Emoji
  -- | Requires MANAGE_EMOJIS permission
  DeleteGuildEmoji :: Snowflake -> Snowflake -> EmojiRequest ()

data ModifyGuildEmojiOpts = ModifyGuildEmojiOpts
     { modifyGuildEmojiName  :: T.Text
     , modifyGuildEmojiRoles :: [Snowflake]
     }

instance ToJSON ModifyGuildEmojiOpts where
  toJSON (ModifyGuildEmojiOpts name roles) =
    object [ "name" .= name, "roles" .= roles ]


data EmojiImage = EmojiImage [Char]

parseEmojiImage :: Q.ByteString -> Either String EmojiImage
parseEmojiImage im = do
      let picE = decodeImage im
          isPic = case picE of Left _ -> False; Right _ -> True
          Right pic = picE
          (w,h) = imageDims pic
      if | not isPic -> Left "Cannot create emoji - bytestring isn't an image"
         | w /= 128 || h /= 128 -> Left "Cannot create emoji - Image isn't 128x128"
         | Q.length im > 256000 -> Left "Cannot create emoji - Image is larger than 256kb"
         | True -> Right (EmojiImage (Q.unpack (B64.encode im)))


emojiMajorRoute :: EmojiRequest a -> String
emojiMajorRoute c = case c of
  (ListGuildEmojis g) ->      "emoji " <> show g
  (GetGuildEmoji g _) ->      "emoji " <> show g
  (CreateGuildEmojiPng g _ _) -> "emoji " <> show g
  (ModifyGuildEmoji g _ _) -> "emoji " <> show g
  (DeleteGuildEmoji g _)   -> "emoji " <> show g

-- | The base url (Req) for API requests
baseUrl :: R.Url 'R.Https
baseUrl = R.https "discordapp.com" R./: "api" R./: apiVersion
  where apiVersion = "v6"

guilds :: R.Url 'R.Https
guilds = baseUrl /: "guilds"

emojiJsonRequest :: EmojiRequest r -> JsonRequest
emojiJsonRequest c = case c of
  (ListGuildEmojis g) -> Get (guilds // g) mempty
  (GetGuildEmoji g e) -> Get (guilds // g /: "emojis" // e) mempty
  (CreateGuildEmojiPng g name (EmojiImage im)) ->
                      Post (guilds // g /: "emojis")
                           (pure (R.ReqBodyJson (object [ "name" .= name
                                                        , "image" .= im
                                                        -- todo , "roles" .= ...
                                                        ])))
                           mempty
  (ModifyGuildEmoji g e o) -> Patch (guilds // g /: "emojis" // e)
                                    (R.ReqBodyJson o)
                                    mempty
  (DeleteGuildEmoji g e) -> Delete (guilds // g /: "emojis" // e) mempty


-- Used internally for parseEmojiImage
imageDims :: DynamicImage -> (Int, Int)
imageDims di =
  case di of
    ImageY8 i -> (imageWidth i, imageHeight i)
    ImageY16 i -> (imageWidth i, imageHeight i)
    ImageYF i -> (imageWidth i, imageHeight i)
    ImageYA8 i -> (imageWidth i, imageHeight i)
    ImageYA16 i -> (imageWidth i, imageHeight i)
    ImageRGB8 i -> (imageWidth i, imageHeight i)
    ImageRGB16 i -> (imageWidth i, imageHeight i)
    ImageRGBF i -> (imageWidth i, imageHeight i)
    ImageRGBA8 i -> (imageWidth i, imageHeight i)
    ImageRGBA16 i -> (imageWidth i, imageHeight i)
    ImageYCbCr8 i -> (imageWidth i, imageHeight i)
    ImageCMYK8 i -> (imageWidth i, imageHeight i)
    ImageCMYK16 i -> (imageWidth i, imageHeight i)
