{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Discord.Internal.Types.Components where

import Data.Aeson
import Data.Data (Data)
import Data.Maybe (fromJust, fromMaybe)
import qualified Data.Text as T
import Data.Tuple (swap)
import Discord.Internal.Types.Prelude (EmojiId, Internals (..), RoleId, makeTable, toMaybeJSON)
import Discord.Internal.Types.User (User)
import qualified Network.HTTP.Req as R

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
      { componentButtonUrl :: R.Url 'R.Https,
        componentButtonDisabled :: Bool,
        componentButtonLabel :: T.Text,
        componentButtonEmoji :: Maybe Emoji
      }
  deriving (Show, Eq)

data ButtonStyle = ButtonStylePrimary | ButtonStyleSecondary | ButtonStyleSuccess | ButtonStyleDanger
  deriving (Show, Eq, Read)

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
  deriving (Show, Eq, Read)

data ComponentActionRow = ComponentActionRowButton [ComponentButton] | ComponentActionRowSelectMenu ComponentSelectMenu
  deriving (Show, Eq)

validPartialEmoji :: Emoji -> Maybe Emoji
validPartialEmoji Emoji {..} = do
  eid <- emojiId
  ean <- emojiAnimated
  return $ Emoji (Just eid) emojiName Nothing Nothing Nothing (Just ean)

instance Internals ComponentActionRow Component where
  toInternal (ComponentActionRowButton as) = Component ComponentTypeActionRow Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing (Just (toInternal' <$> as))
    where
      toInternal' ComponentButtonUrl {..} = Component ComponentTypeButton Nothing (Just componentButtonDisabled) (Just InternalButtonStyleLink) (Just componentButtonLabel) componentButtonEmoji (Just (R.renderUrl componentButtonUrl)) Nothing Nothing Nothing Nothing Nothing
      toInternal' ComponentButton {..} = Component ComponentTypeButton (Just componentButtonCustomId) (Just componentButtonDisabled) (Just (toInternal componentButtonStyle)) (Just componentButtonLabel) componentButtonEmoji Nothing Nothing Nothing Nothing Nothing Nothing
  toInternal (ComponentActionRowSelectMenu ComponentSelectMenu {..}) = Component ComponentTypeActionRow Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing (Just [Component ComponentTypeSelectMenu (Just componentSelectMenuCustomId) (Just componentSelectMenuDisabled) Nothing Nothing Nothing Nothing (Just componentSelectMenuOptions) componentSelectMenuPlaceholder componentSelectMenuMinValues componentSelectMenuMaxValues Nothing])

  fromInternal Component {componentType = ComponentTypeActionRow, componentComponents = (Just (Component {componentType = ComponentTypeSelectMenu, ..} : _))} = do
    cid <- componentCustomId
    cd <- componentDisabled
    co <- componentOptions
    return $ ComponentActionRowSelectMenu $ ComponentSelectMenu cid cd co componentPlaceholder componentMinValues componentMaxValues
  fromInternal Component {componentType = ComponentTypeActionRow, componentComponents = compComps} = compComps >>= mapM fromInternal' >>= Just . ComponentActionRowButton
    where
      fromInternal' Component {componentType = ComponentTypeButton, componentStyle = Just InternalButtonStyleLink, ..} = do
        url <- R.https <$> componentUrl
        label <- componentLabel
        return $ ComponentButtonUrl url (fromMaybe False componentDisabled) label componentEmoji
      fromInternal' Component {componentType = ComponentTypeButton, ..} = do
        customId <- componentCustomId
        label <- componentLabel
        style <- componentStyle >>= fromInternal
        return $ ComponentButton customId (fromMaybe False componentDisabled) style label componentEmoji
      fromInternal' _ = Nothing
  fromInternal _ = Nothing

data Component = Component
  { componentType :: ComponentType,
    -- | Buttons and Select Menus only
    componentCustomId :: Maybe T.Text,
    -- | Buttons and Select Menus only
    componentDisabled :: Maybe Bool,
    -- | Button only
    componentStyle :: Maybe InternalButtonStyle,
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
              ("min_values", toJSON <$> componentMinValues),
              ("max_values", toJSON <$> componentMaxValues),
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

filterOutIncorrectEmoji :: Component -> Component
filterOutIncorrectEmoji c@Component {componentType = ComponentTypeActionRow, componentComponents = (Just cs)} = c {componentComponents = Just (filterOutIncorrectEmoji <$> cs)}
filterOutIncorrectEmoji c@Component {componentType = ComponentTypeSelectMenu, componentOptions = (Just os)} = c {componentOptions = Just ((\so -> so {selectOptionEmoji = selectOptionEmoji so >>= validPartialEmoji}) <$> os)}
filterOutIncorrectEmoji c@Component {componentType = ComponentTypeButton, componentEmoji = (Just e)} = c {componentEmoji = validPartialEmoji e}
filterOutIncorrectEmoji c = c
