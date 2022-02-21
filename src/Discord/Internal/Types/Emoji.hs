{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Discord.Internal.Types.Emoji where

import Data.Aeson
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
    object
      [ (name, value)
        | (name, Just value) <-
            [ ("id", toJSON <$> emojiId),
              ("name", toMaybeJSON emojiName),
              ("roles", toJSON <$> emojiRoles),
              ("user", toJSON <$> emojiUser),
              ("managed", toJSON <$> emojiManaged),
              ("animated", toJSON <$> emojiAnimated)
            ]
      ]
