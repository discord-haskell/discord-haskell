{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Discord.Internal.Types.Components
  ( ComponentActionRow (..),
    ComponentButton (..),
    ButtonStyle (..),
    mkButton,
    ComponentSelectMenu (..),
    mkSelectMenu,
    SelectOption (..),
    mkSelectOption,
    Emoji (..),
    mkEmoji,
  )
where

import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Foldable (Foldable (toList))
import Data.Scientific (Scientific)
import qualified Data.Text as T
import Discord.Internal.Types.Prelude (EmojiId, RoleId, toMaybeJSON)
import Discord.Internal.Types.User (User)

data ComponentActionRow = ComponentActionRowButton [ComponentButton] | ComponentActionRowSelectMenu ComponentSelectMenu
  deriving (Show, Eq, Ord)

instance FromJSON ComponentActionRow where
  parseJSON =
    withObject
      "ComponentActionRow"
      ( \cs -> do
          a <- cs .: "components" :: Parser Array
          let a' = toList a
          case a' of
            [] -> return $ ComponentActionRowButton []
            (c : _) ->
              withObject
                "ComponentActionRow item"
                ( \v -> do
                    t <- v .: "type" :: Parser Int
                    case t of
                      2 -> ComponentActionRowButton <$> mapM parseJSON a'
                      3 -> ComponentActionRowSelectMenu <$> parseJSON c
                      _ -> fail $ "unknown component type: " ++ show t
                )
                c
      )

instance ToJSON ComponentActionRow where
  toJSON (ComponentActionRowButton bs) = object [("type", Number 1), ("components", toJSON bs)]
  toJSON (ComponentActionRowSelectMenu bs) = object [("type", Number 1), ("components", toJSON [bs])]

-- | Component type for a button, split into URL button and not URL button.
--
-- Don't directly send button components - they need to be within an action row.
data ComponentButton
  = ComponentButton
      { -- | Dev indentifier
        componentButtonCustomId :: T.Text,
        -- | Whether the button is disabled
        componentButtonDisabled :: Bool,
        -- | What is the style of the button
        componentButtonStyle :: ButtonStyle,
        -- | What is the user-facing label of the button
        componentButtonLabel :: T.Text,
        -- | What emoji is displayed on the button
        componentButtonEmoji :: Maybe Emoji
      }
  | ComponentButtonUrl
      { -- | The url for the button. If this is not a valid url, everything will
        -- break
        componentButtonUrl :: T.Text,
        -- | Whether the button is disabled
        componentButtonDisabled :: Bool,
        -- | What is the user-facing label of the button
        componentButtonLabel :: T.Text,
        -- | What emoji is displayed on the button
        componentButtonEmoji :: Maybe Emoji
      }
  deriving (Show, Eq, Ord)

-- | Takes the label and the custom id of the button that is to be generated.
mkButton :: T.Text -> T.Text -> ComponentButton
mkButton label customId = ComponentButton customId False ButtonStyleSecondary label Nothing

instance FromJSON ComponentButton where
  parseJSON =
    withObject
      "ComponentButton"
      ( \v -> do
          t <- v .: "type" :: Parser Int
          case t of
            2 -> do
              disabled <- v .:? "disabled" .!= False
              label <- v .: "label"
              partialEmoji <- v .:? "emoji"
              style <- v .: "style" :: Parser Scientific
              case style of
                5 ->
                  ComponentButtonUrl
                    <$> v .: "url"
                    <*> return disabled
                    <*> return label
                    <*> return partialEmoji
                _ ->
                  ComponentButton
                    <$> v .: "custom_id"
                    <*> return disabled
                    <*> parseJSON (Number style)
                    <*> return label
                    <*> return partialEmoji
            _ -> fail "expected button type, got a different component"
      )

instance ToJSON ComponentButton where
  toJSON ComponentButtonUrl {..} =
    object
      [ (name, value)
        | (name, Just value) <-
            [ ("type", Just $ Number 2),
              ("style", Just $ Number 5),
              ("label", toMaybeJSON componentButtonLabel),
              ("disabled", toMaybeJSON componentButtonDisabled),
              ("url", toMaybeJSON componentButtonUrl),
              ("emoji", toJSON <$> componentButtonEmoji)
            ]
      ]
  toJSON ComponentButton {..} =
    object
      [ (name, value)
        | (name, Just value) <-
            [ ("type", Just $ Number 2),
              ("style", Just $ toJSON componentButtonStyle),
              ("label", toMaybeJSON componentButtonLabel),
              ("disabled", toMaybeJSON componentButtonDisabled),
              ("custom_id", toMaybeJSON componentButtonCustomId),
              ("emoji", toJSON <$> componentButtonEmoji)
            ]
      ]

-- | Buttton colors.
data ButtonStyle
  = -- | Blurple button
    ButtonStylePrimary
  | -- | Grey button
    ButtonStyleSecondary
  | -- | Green button
    ButtonStyleSuccess
  | -- | Red button
    ButtonStyleDanger
  deriving (Show, Eq, Ord, Read)

instance FromJSON ButtonStyle where
  parseJSON =
    withScientific
      "ButtonStyle"
      ( \case
          1 -> return ButtonStylePrimary
          2 -> return ButtonStyleSecondary
          3 -> return ButtonStyleSuccess
          4 -> return ButtonStyleDanger
          _ -> fail "unrecognised non-url button style"
      )

instance ToJSON ButtonStyle where
  toJSON ButtonStylePrimary = Number 1
  toJSON ButtonStyleSecondary = Number 2
  toJSON ButtonStyleSuccess = Number 3
  toJSON ButtonStyleDanger = Number 4

-- | Component type for a select menus.
--
-- Don't directly send select menus - they need to be within an action row.
data ComponentSelectMenu = ComponentSelectMenu
  { -- | Dev identifier
    componentSelectMenuCustomId :: T.Text,
    -- | Whether the select menu is disabled
    componentSelectMenuDisabled :: Bool,
    -- | What options are in this select menu (up to 25)
    componentSelectMenuOptions :: [SelectOption],
    -- | Placeholder text if nothing is selected
    componentSelectMenuPlaceholder :: Maybe T.Text,
    -- | Minimum number of values to select (def 1, min 0, max 25)
    componentSelectMenuMinValues :: Maybe Integer,
    -- | Maximum number of values to select (def 1, max 25)
    componentSelectMenuMaxValues :: Maybe Integer
  }
  deriving (Show, Eq, Ord, Read)

-- | Takes the custom id and the options of the select menu that is to be
-- generated.
mkSelectMenu :: T.Text -> [SelectOption] -> ComponentSelectMenu
mkSelectMenu customId sos = ComponentSelectMenu customId False sos Nothing Nothing Nothing

instance FromJSON ComponentSelectMenu where
  parseJSON =
    withObject
      "ComponentSelectMenu"
      ( \v ->
          do
            t <- v .: "type" :: Parser Int
            case t of
              3 ->
                ComponentSelectMenu
                  <$> v .: "custom_id"
                  <*> v .:? "disabled" .!= False
                  <*> v .: "options"
                  <*> v .:? "placeholder"
                  <*> v .:? "min_values"
                  <*> v .:? "max_values"
              _ -> fail "expected select menu type, got different component"
      )

instance ToJSON ComponentSelectMenu where
  toJSON ComponentSelectMenu {..} =
    object
      [ (name, value)
        | (name, Just value) <-
            [ ("type", Just $ Number 3),
              ("custom_id", toMaybeJSON componentSelectMenuCustomId),
              ("disabled", toMaybeJSON componentSelectMenuDisabled),
              ("options", toMaybeJSON componentSelectMenuOptions),
              ("placeholder", toJSON <$> componentSelectMenuPlaceholder),
              ("min_values", toJSON <$> componentSelectMenuMinValues),
              ("max_values", toJSON <$> componentSelectMenuMaxValues)
            ]
      ]

-- | A single option in a select menu.
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

-- | Make a select option from the given label and value.
mkSelectOption :: T.Text -> T.Text -> SelectOption
mkSelectOption label value = SelectOption label value Nothing Nothing Nothing

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
