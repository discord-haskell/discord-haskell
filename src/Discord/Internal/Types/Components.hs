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
import Discord.Internal.Types.Prelude (EmojiId, RoleId, makeTable, toMaybeJSON)
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
    componentUrl :: Maybe T.Text,
    -- | Select Menus only, max 25
    componentOptions :: Maybe [SelectOption],
    -- | Select Menus only, max 100 chars
    componentPlaceholder :: Maybe T.Text,
    -- | Select Menus only, min values to choose, default 1
    componentMinValues :: Maybe Integer,
    -- | Select Menus only, max values to choose, default 1
    componentMaxValues :: Maybe Integer,
    -- | Action Rows only
    componentComponents :: Maybe [Component]
  }
  deriving (Show, Eq, Ord, Read)

instance FromJSON Component where
  parseJSON = withObject "Component" $ \o ->
    Component <$> o .: "type"
      <*> o .:? "custom_id"
      <*> o .:? "disabled"
      <*> o .:? "style"
      <*> o .:? "label"
      <*> o .:? "emoji"
      <*> o .:? "url"
      <*> o .:? "options"
      <*> o .:? "placeholder"
      <*> o .:? "min_values"
      <*> o .:? "max_values"
      <*> o .:? "components"

instance ToJSON Component where
  toJSON Component {..} =
    object
      [ (name, value)
        | (name, Just value) <-
            [ ("type", toMaybeJSON componentType),
              ("custom_id", toJSON <$> componentCustomId),
              ("disabled", toJSON <$> componentDisabled),
              ("style", toJSON <$> componentStyle),
              ("label", toJSON <$> componentLabel),
              ("emoji", toJSON <$> componentEmoji),
              ("url", toJSON <$> componentUrl),
              ("options", toJSON <$> componentOptions),
              ("placeholder", toJSON <$> componentPlaceholder),
              ("min_values", toJSON <$> componentComponents),
              ("max_values", toJSON <$> componentComponents),
              ("components", toJSON <$> componentComponents)
            ]
      ]

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

instance Enum ButtonStyle where
  fromEnum ButtonStylePrimary = 1
  fromEnum ButtonStyleSecondary = 2
  fromEnum ButtonStyleSuccess = 3
  fromEnum ButtonStyleDanger = 4
  fromEnum ButtonStyleLink = 5
  toEnum a = fromJust $ lookup a table
    where
      table = makeTable ButtonStylePrimary

instance ToJSON ButtonStyle where
  toJSON = toJSON . fromEnum

instance FromJSON ButtonStyle where
  parseJSON = withScientific "ButtonStyle" (return . toEnum . round)

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

data SelectOption = SelectOption
  { -- | User facing option name
    selectOptionLabel :: T.Text,
    -- | Dev facing option value
    selectOptionValue :: T.Text,
    -- | additional description
    selectOptionDescription :: Maybe T.Text,
    -- | A partial emoji to show with the object (id, name, animated)
    selectOptionEmoji :: Maybe Emoji,
    -- | Use this value by default
    selectOptionDefault :: Maybe Bool
  }
  deriving (Show, Eq, Ord, Read)

instance FromJSON SelectOption where
  parseJSON = withObject "SelectOption" $ \o ->
    SelectOption <$> o .: "label"
      <*> o .: "value"
      <*> o .:? "description"
      <*> o .:? "emoji"
      <*> o .:? "default"

instance ToJSON SelectOption where
  toJSON SelectOption {..} =
    object
      [ (name, value)
        | (name, Just value) <-
            [ ("label", toMaybeJSON selectOptionLabel),
              ("value", toMaybeJSON selectOptionValue),
              ("description", toJSON <$> selectOptionDescription),
              ("emoji", toJSON <$> selectOptionEmoji),
              ("default", toJSON <$> selectOptionDefault)
            ]
      ]
