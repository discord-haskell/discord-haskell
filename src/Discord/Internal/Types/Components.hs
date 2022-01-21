{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Discord.Internal.Types.Components
  ( ComponentActionRow (..),
    ComponentButton (..),
    ButtonStyle (..),
    ComponentSelectMenu (..),
    SelectOption (..),
    InternalComponentType (..),
    Emoji (..),
    validPartialEmoji,
    filterOutIncorrectEmoji,
  )
where

import Data.Aeson
import Data.Data (Data)
import Data.Maybe (fromJust, fromMaybe)
import qualified Data.Text as T
import Data.Tuple (swap)
import Discord.Internal.Types.Prelude (EmojiId, Internals (..), RoleId, makeTable, toMaybeJSON)
import Discord.Internal.Types.User (User)

-- | Component type for a button, split into URL button and not URL button.
--
-- Don't directly send button components - they need to be within an action row.
data ComponentButton
  = ComponentButton
      { componentButtonCustomId :: T.Text,
        componentButtonDisabled :: Bool,
        componentButtonStyle :: ButtonStyle,
        componentButtonLabel :: T.Text,
        componentButtonEmoji :: Maybe Emoji
      }
  | ComponentButtonUrl
      { -- | The url for the button. If this is not a valid url, everything will
        -- break
        componentButtonUrl :: T.Text,
        componentButtonDisabled :: Bool,
        componentButtonLabel :: T.Text,
        componentButtonEmoji :: Maybe Emoji
      }
  deriving (Show, Eq, Ord)

data ButtonStyle = ButtonStylePrimary | ButtonStyleSecondary | ButtonStyleSuccess | ButtonStyleDanger
  deriving (Show, Eq, Ord, Read)

buttonStyles :: [(ButtonStyle, InternalButtonStyle)]
buttonStyles =
  [ (ButtonStylePrimary, InternalButtonStylePrimary),
    (ButtonStyleSecondary, InternalButtonStyleSecondary),
    (ButtonStyleSuccess, InternalButtonStyleSuccess),
    (ButtonStyleDanger, InternalButtonStyleDanger)
  ]

instance Internals ButtonStyle InternalButtonStyle where
  toInternal a = fromJust (lookup a buttonStyles)
  fromInternal b = lookup b (swap <$> buttonStyles)

-- | Component type for a select menus.
--
-- Don't directly send select menus - they need to be within an action row.
data ComponentSelectMenu = ComponentSelectMenu
  { componentSelectMenuCustomId :: T.Text,
    componentSelectMenuDisabled :: Bool,
    componentSelectMenuOptions :: [SelectOption],
    componentSelectMenuPlaceholder :: Maybe T.Text,
    componentSelectMenuMinValues :: Maybe Integer,
    componentSelectMenuMaxValues :: Maybe Integer
  }
  deriving (Show, Eq, Ord, Read)

data ComponentActionRow = ComponentActionRowButton [ComponentButton] | ComponentActionRowSelectMenu ComponentSelectMenu
  deriving (Show, Eq, Ord)

instance FromJSON ComponentActionRow where
  parseJSON v = do
    iac <- parseJSON @InternalComponent v
    case fromInternal iac of
      Nothing -> fail "could not convert internal component"
      Just v' -> return v'

instance ToJSON ComponentActionRow where
  toJSON v = toJSON @InternalComponent $ toInternal v

validPartialEmoji :: Emoji -> Maybe Emoji
validPartialEmoji Emoji {..} = do
  eid <- emojiId
  ean <- emojiAnimated
  return $ Emoji (Just eid) emojiName Nothing Nothing Nothing (Just ean)

instance Internals ComponentActionRow InternalComponent where
  toInternal (ComponentActionRowButton as) = InternalComponent InternalComponentTypeActionRow Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing (Just (toInternal' <$> as))
    where
      toInternal' ComponentButtonUrl {..} = InternalComponent InternalComponentTypeButton Nothing (Just componentButtonDisabled) (Just InternalButtonStyleLink) (Just componentButtonLabel) componentButtonEmoji (Just componentButtonUrl) Nothing Nothing Nothing Nothing Nothing
      toInternal' ComponentButton {..} = InternalComponent InternalComponentTypeButton (Just componentButtonCustomId) (Just componentButtonDisabled) (Just (toInternal componentButtonStyle)) (Just componentButtonLabel) componentButtonEmoji Nothing Nothing Nothing Nothing Nothing Nothing
  toInternal (ComponentActionRowSelectMenu ComponentSelectMenu {..}) = InternalComponent InternalComponentTypeActionRow Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing (Just [InternalComponent InternalComponentTypeSelectMenu (Just componentSelectMenuCustomId) (Just componentSelectMenuDisabled) Nothing Nothing Nothing Nothing (Just componentSelectMenuOptions) componentSelectMenuPlaceholder componentSelectMenuMinValues componentSelectMenuMaxValues Nothing])

  fromInternal InternalComponent {internalComponentType = InternalComponentTypeActionRow, internalComponentComponents = (Just [InternalComponent {internalComponentType = InternalComponentTypeSelectMenu, ..}])} = do
    cid <- internalComponentCustomId
    co <- internalComponentOptions
    return $ ComponentActionRowSelectMenu $ ComponentSelectMenu cid (fromMaybe False internalComponentDisabled) co internalComponentPlaceholder internalComponentMinValues internalComponentMaxValues
  fromInternal InternalComponent {internalComponentType = InternalComponentTypeActionRow, internalComponentComponents = compComps} = compComps >>= mapM fromInternal' >>= Just . ComponentActionRowButton
    where
      fromInternal' InternalComponent {internalComponentType = InternalComponentTypeButton, internalComponentStyle = Just InternalButtonStyleLink, ..} = do
        url <- internalComponentUrl
        label <- internalComponentLabel
        return $ ComponentButtonUrl url (fromMaybe False internalComponentDisabled) label internalComponentEmoji
      fromInternal' InternalComponent {internalComponentType = InternalComponentTypeButton, ..} = do
        customId <- internalComponentCustomId
        label <- internalComponentLabel
        style <- internalComponentStyle >>= fromInternal
        return $ ComponentButton customId (fromMaybe False internalComponentDisabled) style label internalComponentEmoji
      fromInternal' _ = Nothing
  fromInternal _ = Nothing

data InternalComponent = InternalComponent
  { internalComponentType :: InternalComponentType,
    -- | Buttons and Select Menus only
    internalComponentCustomId :: Maybe T.Text,
    -- | Buttons and Select Menus only
    internalComponentDisabled :: Maybe Bool,
    -- | Button only
    internalComponentStyle :: Maybe InternalButtonStyle,
    -- | Button only
    internalComponentLabel :: Maybe T.Text,
    -- | Button only
    internalComponentEmoji :: Maybe Emoji,
    -- | Button only, link buttons only
    internalComponentUrl :: Maybe T.Text,
    -- | Select Menus only, max 25
    internalComponentOptions :: Maybe [SelectOption],
    -- | Select Menus only, max 100 chars
    internalComponentPlaceholder :: Maybe T.Text,
    -- | Select Menus only, min values to choose, default 1
    internalComponentMinValues :: Maybe Integer,
    -- | Select Menus only, max values to choose, default 1
    internalComponentMaxValues :: Maybe Integer,
    -- | Action Rows only
    internalComponentComponents :: Maybe [InternalComponent]
  }
  deriving (Show, Eq, Ord, Read)

instance FromJSON InternalComponent where
  parseJSON = withObject "InternalComponent" $ \o ->
    InternalComponent <$> o .: "type"
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

instance ToJSON InternalComponent where
  toJSON InternalComponent {..} =
    object
      [ (name, value)
        | (name, Just value) <-
            [ ("type", toMaybeJSON internalComponentType),
              ("custom_id", toJSON <$> internalComponentCustomId),
              ("disabled", toJSON <$> internalComponentDisabled),
              ("style", toJSON <$> internalComponentStyle),
              ("label", toJSON <$> internalComponentLabel),
              ("emoji", toJSON <$> internalComponentEmoji),
              ("url", toJSON <$> internalComponentUrl),
              ("options", toJSON <$> internalComponentOptions),
              ("placeholder", toJSON <$> internalComponentPlaceholder),
              ("min_values", toJSON <$> internalComponentMinValues),
              ("max_values", toJSON <$> internalComponentMaxValues),
              ("components", toJSON <$> internalComponentComponents)
            ]
      ]

-- | The different types of components
data InternalComponentType
  = -- | A container for other components
    InternalComponentTypeActionRow
  | -- | A button
    InternalComponentTypeButton
  | -- | A select menu for picking from choices
    InternalComponentTypeSelectMenu
  deriving (Show, Read, Eq, Ord, Data)

instance Enum InternalComponentType where
  fromEnum InternalComponentTypeActionRow = 1
  fromEnum InternalComponentTypeButton = 2
  fromEnum InternalComponentTypeSelectMenu = 3
  toEnum a = fromJust $ lookup a table
    where
      table = makeTable InternalComponentTypeActionRow

instance ToJSON InternalComponentType where
  toJSON = toJSON . fromEnum

instance FromJSON InternalComponentType where
  parseJSON = withScientific "InternalComponentType" (return . toEnum . round)

data InternalButtonStyle
  = -- | Blurple button
    InternalButtonStylePrimary
  | -- | Grey button
    InternalButtonStyleSecondary
  | -- | Green button
    InternalButtonStyleSuccess
  | -- | Red button
    InternalButtonStyleDanger
  | -- | Grey button, navigates to URL
    InternalButtonStyleLink
  deriving (Show, Read, Eq, Ord, Data)

instance Enum InternalButtonStyle where
  fromEnum InternalButtonStylePrimary = 1
  fromEnum InternalButtonStyleSecondary = 2
  fromEnum InternalButtonStyleSuccess = 3
  fromEnum InternalButtonStyleDanger = 4
  fromEnum InternalButtonStyleLink = 5
  toEnum a = fromJust $ lookup a table
    where
      table = makeTable InternalButtonStylePrimary

instance ToJSON InternalButtonStyle where
  toJSON = toJSON . fromEnum

instance FromJSON InternalButtonStyle where
  parseJSON = withScientific "InternalButtonStyle" (return . toEnum . round)

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

filterOutIncorrectEmoji :: InternalComponent -> InternalComponent
filterOutIncorrectEmoji c@InternalComponent {internalComponentType = InternalComponentTypeActionRow, internalComponentComponents = (Just cs)} = c {internalComponentComponents = Just (filterOutIncorrectEmoji <$> cs)}
filterOutIncorrectEmoji c@InternalComponent {internalComponentType = InternalComponentTypeSelectMenu, internalComponentOptions = (Just os)} = c {internalComponentOptions = Just ((\so -> so {selectOptionEmoji = selectOptionEmoji so >>= validPartialEmoji}) <$> os)}
filterOutIncorrectEmoji c@InternalComponent {internalComponentType = InternalComponentTypeButton, internalComponentEmoji = (Just e)} = c {internalComponentEmoji = validPartialEmoji e}
filterOutIncorrectEmoji c = c
