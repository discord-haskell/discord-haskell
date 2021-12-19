{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Discord.Internal.Types.Components where

import Data.Aeson
import Data.Data (Data)
import Data.Maybe (fromJust)
import qualified Data.Text as T
import Discord.Internal.Types.Prelude (EmojiId, RoleId, makeTable)
import Discord.Internal.Types.User (User)

data Component = Component
  { componentType :: ComponentType,
    -- | Buttons and Select Menus only
    componentCustomId :: Maybe T.Text,
    -- | Buttons and Select Menus only
    componentDisabled :: Maybe Bool,
    -- | Button only
    componentStyle :: Maybe ButtonStyle,
    -- | Button only
    componentLabel :: Maybe T.Text,
    -- | Button only
    componentEmoji :: Maybe Emoji,
    -- | Button only, link buttons only
    componentUrl :: Maybe T.Text
  }
  deriving (Show, Eq, Ord, Read)

-- | The different types of components
data ComponentType
  = -- | A container for other components
    ComponentTypeActionRow
  | -- | A button
    ComponentTypeButton
  | -- | A select menu for picking from choices
    ComponentTypeSelectMenu
  deriving (Show, Read, Eq, Ord, Data)

instance Enum ComponentType where
  fromEnum ComponentTypeActionRow = 1
  fromEnum ComponentTypeButton = 2
  fromEnum ComponentTypeSelectMenu = 3
  toEnum a = fromJust $ lookup a table
    where
      table = makeTable ComponentTypeActionRow

instance ToJSON ComponentType where
  toJSON = toJSON . fromEnum

instance FromJSON ComponentType where
  parseJSON = withScientific "StickerFormatType" (return . toEnum . round)

data ButtonStyle
  = -- | Blurple button
    ButtonStylePrimary
  | -- | Grey button
    ButtonStyleSecondary
  | -- | Green button
    ButtonStyleSuccess
  | -- | Red button
    ButtonStyleDanger
  | -- | Grey button, navigates to URL
    ButtonStyleLink
  deriving (Show, Read, Eq, Ord, Data)

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

instance FromJSON Emoji where
  parseJSON = withObject "Emoji" $ \o ->
    Emoji <$> o .: "id"
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
              ("name", toJSON <$> pure emojiName),
              ("roles", toJSON <$> emojiRoles),
              ("user", toJSON <$> emojiUser),
              ("managed", toJSON <$> emojiManaged),
              ("animated", toJSON <$> emojiAnimated)
            ]
      ]
