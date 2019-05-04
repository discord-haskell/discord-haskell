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
  ListGuildEmojis :: GuildId -> EmojiRequest [Emoji]
  -- | Emoji object for the given guild and emoji ID
  GetGuildEmoji :: GuildId -> EmojiId -> EmojiRequest Emoji
  -- | Create a new guild emoji (static&animated). Requires MANAGE_EMOJIS permission.
  CreateGuildEmoji :: GuildId -> T.Text -> EmojiImageParsed -> EmojiRequest Emoji
  -- | Requires MANAGE_EMOJIS permission
  ModifyGuildEmoji :: GuildId -> EmojiId -> ModifyGuildEmojiOpts -> EmojiRequest Emoji
  -- | Requires MANAGE_EMOJIS permission
  DeleteGuildEmoji :: GuildId -> EmojiId -> EmojiRequest ()

data ModifyGuildEmojiOpts = ModifyGuildEmojiOpts
     { modifyGuildEmojiName  :: T.Text
     , modifyGuildEmojiRoles :: [RoleId]
     }

instance ToJSON ModifyGuildEmojiOpts where
  toJSON (ModifyGuildEmojiOpts name roles) =
    object [ "name" .= name, "roles" .= roles ]


data EmojiImageParsed = EmojiImageParsed String

parseEmojiImage :: Q.ByteString -> Either String EmojiImageParsed
parseEmojiImage bs =
  if Q.length bs > 256000
  then Left "Cannot create emoji - File is larger than 256kb"
  else case (decodeGifImages bs, decodeImage bs) of
         (Left e1, Left e2) -> Left ("Could not parse image or gif: " <> e1
                                                           <> " and " <> e2)
         (Right ims, _) -> if all is128 ims
                           then Right (EmojiImageParsed ("data:text/plain;"
                                                      <> "base64,"
                                                      <> Q.unpack (B64.encode bs)))
                           else Left ("The frames are not all 128x128")
         (_, Right im) -> if is128 im
                          then Right (EmojiImageParsed ("data:text/plain;"
                                                     <> "base64,"
                                                     <> Q.unpack (B64.encode bs)))
                          else Left ("Image is not 128x128")
  where
    is128 im = let i = convertRGB8 im
               in imageWidth i == 128 && imageHeight i == 128


emojiMajorRoute :: EmojiRequest a -> String
emojiMajorRoute c = case c of
  (ListGuildEmojis g) ->      "emoji " <> show g
  (GetGuildEmoji g _) ->      "emoji " <> show g
  (CreateGuildEmoji g _ _) -> "emoji " <> show g
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
  (CreateGuildEmoji g name (EmojiImageParsed im)) ->
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
