{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Message components
module Discord.Internal.Types.Components
  ( ActionRow (..),
    Button (..),
    ButtonStyle (..),
    mkButton,
    SelectMenu (..),
    mkSelectMenu,
    SelectMenuData (..),
    SelectOption (..),
    mkSelectOption,
    TextInput (..),
    mkTextInput,
  )
where

import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Foldable (Foldable (toList))
import Data.Scientific (Scientific)
import qualified Data.Text as T
import Discord.Internal.Types.Emoji (Emoji)
import Discord.Internal.Types.Prelude (objectFromMaybes, (.==), (.=?), ChannelTypeOption)

-- | Container for other message Components
data ActionRow = ActionRowButtons [Button] | ActionRowSelectMenu SelectMenu
  deriving (Show, Read, Eq, Ord)

instance FromJSON ActionRow where
  parseJSON =
    withObject
      "ActionRow"
      ( \cs -> do
          t <- cs .: "type" :: Parser Int
          case t of
            1 -> do
              a <- cs .: "components" :: Parser Array
              let a' = toList a
              case a' of
                [] -> return $ ActionRowButtons []
                (c : _) ->
                  withObject
                    "ActionRow item"
                    ( \v -> do
                        t' <- v .: "type" :: Parser Int
                        case t' of
                          2 -> ActionRowButtons <$> mapM parseJSON a'
                          _ | t' `elem` [3, 5, 6, 7, 8] -> ActionRowSelectMenu <$> parseJSON c
                          _ -> fail $ "unknown component type: " ++ show t'
                    )
                    c
            _ -> fail $ "expected action row type (1), got: " ++ show t
      )

instance ToJSON ActionRow where
  toJSON (ActionRowButtons bs) = object [("type", Number 1), ("components", toJSON bs)]
  toJSON (ActionRowSelectMenu bs) = object [("type", Number 1), ("components", toJSON [bs])]

-- | Component type for a button, split into URL button and not URL button.
--
-- Don't directly send button components - they need to be within an action row.
data Button
  = Button
      { -- | Dev indentifier
        buttonCustomId :: T.Text,
        -- | Whether the button is disabled
        buttonDisabled :: Bool,
        -- | What is the style of the button
        buttonStyle :: ButtonStyle,
        -- | What is the user-facing label of the button
        buttonLabel :: Maybe T.Text,
        -- | What emoji is displayed on the button
        buttonEmoji :: Maybe Emoji
      }
  | ButtonUrl
      { -- | The url for the button. If this is not a valid url, everything will
        -- break
        buttonUrl :: T.Text,
        -- | Whether the button is disabled
        buttonDisabled :: Bool,
        -- | What is the user-facing label of the button
        buttonLabel :: Maybe T.Text,
        -- | What emoji is displayed on the button
        buttonEmoji :: Maybe Emoji
      }
  deriving (Show, Read, Eq, Ord)

-- | Takes the label and the custom id of the button that is to be generated.
mkButton :: T.Text -> T.Text -> Button
mkButton label customId = Button customId False ButtonStyleSecondary (Just label) Nothing

instance FromJSON Button where
  parseJSON =
    withObject
      "Button"
      ( \v -> do
          t <- v .: "type" :: Parser Int
          case t of
            2 -> do
              disabled <- v .:? "disabled" .!= False
              label <- v .:? "label"
              partialEmoji <- v .:? "emoji"
              style <- v .: "style" :: Parser Scientific
              case style of
                5 ->
                  ButtonUrl
                    <$> v .: "url"
                    <*> return disabled
                    <*> return label
                    <*> return partialEmoji
                _ ->
                  Button
                    <$> v .: "custom_id"
                    <*> return disabled
                    <*> parseJSON (Number style)
                    <*> return label
                    <*> return partialEmoji
            _ -> fail "expected button type, got a different component"
      )

instance ToJSON Button where
  toJSON ButtonUrl {..} =
    objectFromMaybes
      [ "type" .== Number 2,
        "style" .== Number 5,
        "label" .=? buttonLabel,
        "disabled" .== buttonDisabled,
        "url" .== buttonUrl,
        "emoji" .=? buttonEmoji
      ]
  toJSON Button {..} =
    objectFromMaybes
      [ "type" .== Number 2,
        "style" .== buttonStyle,
        "label" .=? buttonLabel,
        "disabled" .== buttonDisabled,
        "custom_id" .== buttonCustomId,
        "emoji" .=? buttonEmoji
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
  deriving (Show, Read, Eq, Ord)

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

-- | Component type for a select menu.
--
-- Don't directly send select menus - they need to be within an action row.
data SelectMenu = SelectMenu
  { -- | Dev identifier
    selectMenuCustomId :: T.Text,
    -- | Whether the select menu is disabled
    selectMenuDisabled :: Bool,
    -- | What type this select menu is, and the data it can hold
    selectMenuData :: SelectMenuData,
    -- | Placeholder text if nothing is selected
    selectMenuPlaceholder :: Maybe T.Text,
    -- | Minimum number of values to select (def 1, min 0, max 25)
    selectMenuMinValues :: Maybe Integer,
    -- | Maximum number of values to select (def 1, max 25)
    selectMenuMaxValues :: Maybe Integer
  }
  deriving (Show, Read, Eq, Ord)

-- | Takes the custom id and the options of the select menu that is to be
-- generated.
mkSelectMenu :: T.Text -> [SelectOption] -> SelectMenu
mkSelectMenu customId sos = SelectMenu customId False (SelectMenuDataText sos) Nothing Nothing Nothing

instance FromJSON SelectMenu where
  parseJSON =
    withObject
      "SelectMenu"
      $ \v ->
          do
                SelectMenu
                  <$> v .: "custom_id"
                  <*> v .:? "disabled" .!= False
                  <*> parseJSON (Object v)
                  <*> v .:? "placeholder"
                  <*> v .:? "min_values"
                  <*> v .:? "max_values"
      

instance ToJSON SelectMenu where
  toJSON SelectMenu {..} =
    objectFromMaybes $
      [ "custom_id" .== selectMenuCustomId,
        "disabled" .== selectMenuDisabled,
        "placeholder" .=? selectMenuPlaceholder,
        "min_values" .=? selectMenuMinValues,
        "max_values" .=? selectMenuMaxValues
      ] <> case selectMenuData of
            SelectMenuDataText sos -> ["type" .== Number 3, "options" .== sos]
            SelectMenuDataUser -> ["type" .== Number 5]
            SelectMenuDataRole -> ["type" .== Number 6]
            SelectMenuDataMentionable -> ["type" .== Number 7]
            SelectMenuDataChannels ctos -> ["type" .== Number 8, "channel_types" .== ctos]

data SelectMenuData = 
    SelectMenuDataText [SelectOption] -- ^ Text options
  | SelectMenuDataUser -- ^ Users
  | SelectMenuDataRole -- ^ Roles
  | SelectMenuDataMentionable -- ^ Anything mentionable (users and roles)
  | SelectMenuDataChannels [ChannelTypeOption] -- ^ Channels (of certain types)
  deriving (Show, Read, Eq, Ord)

instance FromJSON SelectMenuData where
  parseJSON =
    withObject "SelectMenuData" $ \v ->
      do
        t <- v .: "type"
        case t::Int of
          3 -> SelectMenuDataText <$> v .: "options"
          5 -> pure SelectMenuDataUser
          6 -> pure SelectMenuDataRole
          7 -> pure SelectMenuDataMentionable
          8 -> SelectMenuDataChannels <$> v .: "channel_types"
          _ -> fail ("unknown select menu data type: " <> show t)

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
  deriving (Show, Read, Eq, Ord)

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
    objectFromMaybes
      [ "label" .== selectOptionLabel,
        "value" .== selectOptionValue,
        "description" .=? selectOptionDescription,
        "emoji" .=? selectOptionEmoji,
        "default" .=? selectOptionDefault
      ]

data TextInput = TextInput
  { -- | Dev identifier
    textInputCustomId :: T.Text,
    -- | What style to use (short or paragraph)
    textInputIsParagraph :: Bool,
    -- | The label for this component
    textInputLabel :: T.Text,
    -- | The minimum input length for a text input (0-4000)
    textInputMinLength :: Maybe Integer,
    -- | The maximum input length for a text input (1-4000)
    textInputMaxLength :: Maybe Integer,
    -- | Whether this component is required to be filled
    textInputRequired :: Bool,
    -- | The prefilled value for this component (max 4000)
    textInputValue :: T.Text,
    -- | Placeholder text if empty (max 4000)
    textInputPlaceholder :: T.Text
  }
  deriving (Show, Read, Eq, Ord)

instance ToJSON TextInput where
  toJSON TextInput {..} =
    objectFromMaybes
      [ "type" .== Number 4,
        "custom_id" .== textInputCustomId,
        "style" .== (1 + fromEnum textInputIsParagraph),
        "label" .== textInputLabel,
        "min_length" .=? textInputMinLength,
        "max_length" .=? textInputMaxLength,
        "required" .== textInputRequired,
        "value" .== textInputValue,
        "placeholder" .== textInputPlaceholder
      ]

instance FromJSON TextInput where
  parseJSON = withObject "TextInput" $ \o -> do
    t <- o .: "type" :: Parser Int
    case t of
      4 ->
        TextInput <$> o .: "custom_id"
          <*> fmap (== (2 :: Int)) (o .:? "style" .!= 1)
          <*> o .:? "label" .!= ""
          <*> o .:? "min_length"
          <*> o .:? "max_length"
          <*> o .:? "required" .!= False
          <*> o .:? "value" .!= ""
          <*> o .:? "placeholder" .!= ""
      _ -> fail "expected text input, found other type of component"

-- | Create a text input from an id and a label
mkTextInput :: T.Text -> T.Text -> TextInput
mkTextInput cid label = TextInput cid False label Nothing Nothing True "" ""
