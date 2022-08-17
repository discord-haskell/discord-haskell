{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module Discord.Internal.Types.ApplicationCommands
  ( ApplicationCommand (..),
    Options (..),
    OptionSubcommandOrGroup (..),
    OptionSubcommand (..),
    OptionValue (..),
    createChatInput,
    createUser,
    createMessage,
    CreateApplicationCommand (..),
    EditApplicationCommand (..),
    defaultEditApplicationCommand,
    Choice (..),
    ApplicationCommandChannelType (..),
    GuildApplicationCommandPermissions (..),
    ApplicationCommandPermissions (..),
    Number,
    AutocompleteOrChoice,
  )
where

import Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON), Value (Number, Object), object, withArray, withObject, (.!=), (.:), (.:!), (.:?))
import Data.Aeson.Types (Pair, Parser)
import Data.Data (Data)
import Data.Foldable (Foldable (toList))
import Data.Scientific (Scientific)
import qualified Data.Text as T
import Discord.Internal.Types.Prelude (ApplicationCommandId, ApplicationId, GuildId, InternalDiscordEnum (..), Snowflake, discordTypeParseJSON, objectFromMaybes, objectToList, (.==), (.=?))
import Data.Bifunctor (Bifunctor(second))

type Number = Scientific

-- | The structure for an application command.
data ApplicationCommand
  = ApplicationCommandUser
      { -- | The id of the application command.
        applicationCommandId :: ApplicationCommandId,
        -- | The id of the application the command comes from.
        applicationCommandApplicationId :: ApplicationId,
        -- | The guild the application command is registered in.
        applicationCommandGuildId :: Maybe GuildId,
        -- | The name of the application command.
        applicationCommandName :: T.Text,
        -- | The localized names of the application command.
        applicationCommandLocalizedName :: Maybe LocalizedText,
        -- | Whether the command is enabled by default when the app is added to a guild.
        applicationCommandDefaultPermission :: Bool,
        -- | Autoincrementing version identifier updated during substantial record changes.
        applicationCommandVersion :: Snowflake
      }
  | ApplicationCommandMessage
      { -- | The id of the application command.
        applicationCommandId :: ApplicationCommandId,
        -- | The id of the application the command comes from.
        applicationCommandApplicationId :: ApplicationId,
        -- | The guild the application command is registered in.
        applicationCommandGuildId :: Maybe GuildId,
        -- | The name of the application command.
        applicationCommandName :: T.Text,
        -- | The localized names of the application command.
        applicationCommandLocalizedName :: Maybe LocalizedText,
        -- | Whether the command is enabled by default when the app is added to a guild.
        applicationCommandDefaultPermission :: Bool,
        -- | Autoincrementing version identifier updated during substantial record changes.
        applicationCommandVersion :: Snowflake
      }
  | ApplicationCommandChatInput
      { -- | The id of the application command.
        applicationCommandId :: ApplicationCommandId,
        -- | The id of the application the command comes from.
        applicationCommandApplicationId :: ApplicationId,
        -- | The guild the application command is registered in.
        applicationCommandGuildId :: Maybe GuildId,
        -- | The name of the application command.
        applicationCommandName :: T.Text,
        -- | The localized names of the application command.
        applicationLocalizedName :: Maybe LocalizedText,
        -- | The description of the application command.
        applicationCommandDescription :: T.Text,
        -- | The localized descriptions of the application command.
        applicationCommandLocalizedDescription :: Maybe LocalizedText,
        -- | The parameters for the command.
        applicationCommandOptions :: Maybe Options,
        -- | Whether the command is enabled by default when the app is added to a guild.
        applicationCommandDefaultPermission :: Bool,
        -- | Autoincrementing version identifier updated during substantial record changes.
        applicationCommandVersion :: Snowflake
      }
  deriving (Show, Eq, Read)

instance FromJSON ApplicationCommand where
  parseJSON =
    withObject
      "ApplicationCommand"
      ( \v -> do
          acid <- v .: "id"
          aid <- v .: "application_id"
          gid <- v .:? "guild_id"
          name <- v .: "name"
          lname <- v .:? "name_localizations"
          defPerm <- v .:? "default_permission" .!= True
          version <- v .: "version"
          t <- v .:? "type" :: Parser (Maybe Int)
          case t of
            (Just 2) -> return $ ApplicationCommandUser acid aid gid name lname defPerm version
            (Just 3) -> return $ ApplicationCommandMessage acid aid gid name lname defPerm version
            _ -> do
              desc <- v .: "description"
              options <- v .:? "options"
              ldesc <- v .: "description_localizations"
              return $ ApplicationCommandChatInput acid aid gid name lname desc ldesc options defPerm version
      )

-- | Either subcommands and groups, or values.
data Options
  = OptionsSubcommands [OptionSubcommandOrGroup]
  | OptionsValues [OptionValue]
  deriving (Show, Eq, Read)

instance FromJSON Options where
  parseJSON =
    withArray
      "Options"
      ( \a -> do
          let a' = toList a
          case a' of
            [] -> return $ OptionsValues []
            (v' : _) ->
              withObject
                "Options item"
                ( \v -> do
                    t <- v .: "type" :: Parser Int
                    if t == 1 || t == 2
                      then OptionsSubcommands <$> mapM parseJSON a'
                      else OptionsValues <$> mapM parseJSON a'
                )
                v'
      )

instance ToJSON Options where
  toJSON (OptionsSubcommands o) = toJSON o
  toJSON (OptionsValues o) = toJSON o

-- | Either a subcommand group or a subcommand.
data OptionSubcommandOrGroup
  = OptionSubcommandGroup
      { -- | The name of the subcommand group
        optionSubcommandGroupName :: T.Text,
        -- | The localized name of the subcommand group
        optionSubcommandGroupLocalizedName :: Maybe LocalizedText,
        -- | The description of the subcommand group
        optionSubcommandGroupDescription :: T.Text,
        -- | The localized description of the subcommand group
        optionSubcommandGroupLocalizedDescription :: Maybe LocalizedText,
        -- | The subcommands in this subcommand group
        optionSubcommandGroupOptions :: [OptionSubcommand]
      }
  | OptionSubcommandOrGroupSubcommand OptionSubcommand
  deriving (Show, Eq, Read)

instance FromJSON OptionSubcommandOrGroup where
  parseJSON =
    withObject
      "OptionSubcommandOrGroup"
      ( \v -> do
          t <- v .: "type" :: Parser Int
          case t of
            2 ->
              OptionSubcommandGroup
                <$> v .: "name"
                <*> v .:? "name_localizations"
                <*> v .: "description"
                <*> v .:? "description_localizations"
                <*> v .: "options"
            1 -> OptionSubcommandOrGroupSubcommand <$> parseJSON (Object v)
            _ -> fail "unexpected subcommand group type"
      )

instance ToJSON OptionSubcommandOrGroup where
  toJSON OptionSubcommandGroup {..} =
    object
      [ ("type", Number 2),
        ("name", toJSON optionSubcommandGroupName),
        ("name_localizations", toJSON optionSubcommandGroupLocalizedName),
        ("description", toJSON optionSubcommandGroupDescription),
        ("description_localizations", toJSON optionSubcommandGroupLocalizedDescription),
        ("options", toJSON optionSubcommandGroupOptions)
      ]
  toJSON (OptionSubcommandOrGroupSubcommand a) = toJSON a

-- | Data for a single subcommand.
data OptionSubcommand = OptionSubcommand
  { -- | The name of the subcommand
    optionSubcommandName :: T.Text,
    -- | The localized name of the subcommand
    optionSubcommandLocalizedName :: Maybe LocalizedText,
    -- | The description of the subcommand
    optionSubcommandDescription :: T.Text,
    -- | The localized description of the subcommand
    optionSubcommandLocalizedDescription :: Maybe LocalizedText,
    -- | What options are there in this subcommand
    optionSubcommandOptions :: [OptionValue]
  }
  deriving (Show, Eq, Read)

instance FromJSON OptionSubcommand where
  parseJSON =
    withObject
      "OptionSubcommand"
      ( \v -> do
          t <- v .: "type" :: Parser Int
          case t of
            1 ->
              OptionSubcommand
                <$> v .: "name"
                <*> v .:? "name_localizations"
                <*> v .: "description"
                <*> v .:? "description_localizations"
                <*> v .:? "options" .!= []
            _ -> fail "unexpected subcommand type"
      )

instance ToJSON OptionSubcommand where
  toJSON OptionSubcommand {..} =
    object
      [ ("type", Number 1),
        ("name", toJSON optionSubcommandName),
        ("name_localizations", toJSON optionSubcommandLocalizedName),
        ("description", toJSON optionSubcommandDescription),
        ("description_localizations", toJSON optionSubcommandLocalizedDescription),
        ("options", toJSON optionSubcommandOptions)
      ]

-- | Data for a single value.
data OptionValue
  = OptionValueString
      { -- | The name of the value
        optionValueName :: T.Text,
        -- | The localized name of the value
        optionValueLocalizedName :: Maybe LocalizedText,
        -- | The description of the value
        optionValueDescription :: T.Text,
        -- | The localized description of the value
        optionValueLocalizedDescription :: Maybe LocalizedText,
        -- | Whether this option is required
        optionValueRequired :: Bool,
        -- | Whether to autocomplete or have a list of named choices. For neither option, use `Left False`
        optionValueStringChoices :: AutocompleteOrChoice T.Text,
        -- | The minimum length of the string (minimum 0)
        optionValueStringMinLen :: Maybe Integer,
        -- | The maximum length of the string (minimum 1)
        optionValueStringMaxLen :: Maybe Integer
      }
  | OptionValueInteger
      { -- | The name of the value
        optionValueName :: T.Text,
        -- | The localized name of the value
        optionValueLocalizedName :: Maybe LocalizedText,
        -- | The description of the value
        optionValueDescription :: T.Text,
        -- | The localized description of the value
        optionValueLocalizedDescription :: Maybe LocalizedText,
        -- | Whether this option is required
        optionValueRequired :: Bool,
        -- | Whether to autocomplete or have a list of named choices. For neither option, use `Left False`
        optionValueIntegerChoices :: AutocompleteOrChoice Integer,
        -- | The lower bound of values permitted. If choices are provided or autocomplete is on, this can be ignored
        optionValueIntegerMinVal :: Maybe Integer,
        -- | The upper bound of values permitted. If choices are provided or autocomplete is on, this can be ignored
        optionValueIntegerMaxVal :: Maybe Integer
      }
  | OptionValueBoolean
      { -- | The name of the value
        optionValueName :: T.Text,
        -- | The localized name of the value
        optionValueLocalizedName :: Maybe LocalizedText,
        -- | The description of the value
        optionValueDescription :: T.Text,
        -- | The localized description of the value
        optionValueLocalizedDescription :: Maybe LocalizedText,
        -- | Whether this option is required
        optionValueRequired :: Bool
      }
  | OptionValueUser
      { -- | The name of the value
        optionValueName :: T.Text,
        -- | The localized name of the value
        optionValueLocalizedName :: Maybe LocalizedText,
        -- | The description of the value
        optionValueDescription :: T.Text,
        -- | The localized description of the value
        optionValueLocalizedDescription :: Maybe LocalizedText,
        -- | Whether this option is required
        optionValueRequired :: Bool
      }
  | OptionValueChannel
      { -- | The name of the value
        optionValueName :: T.Text,
        -- | The localized name of the value
        optionValueLocalizedName :: Maybe LocalizedText,
        -- | The description of the value
        optionValueDescription :: T.Text,
        -- | The localized description of the value
        optionValueLocalizedDescription :: Maybe LocalizedText,
        -- | Whether this option is required
        optionValueRequired :: Bool,
        -- | What type of channel can be put in here
        optionValueChannelTypes :: Maybe [ApplicationCommandChannelType]
      }
  | OptionValueRole
      { -- | The name of the value
        optionValueName :: T.Text,
        -- | The localized name of the value
        optionValueLocalizedName :: Maybe LocalizedText,
        -- | The description of the value
        optionValueDescription :: T.Text,
        -- | The localized description of the value
        optionValueLocalizedDescription :: Maybe LocalizedText,
        -- | Whether this option is required
        optionValueRequired :: Bool
      }
  | OptionValueMentionable
      { -- | The name of the value
        optionValueName :: T.Text,
        -- | The localized name of the value
        optionValueLocalizedName :: Maybe LocalizedText,
        -- | The description of the value
        optionValueDescription :: T.Text,
        -- | The localized description of the value
        optionValueLocalizedDescription :: Maybe LocalizedText,
        -- | Whether this option is required
        optionValueRequired :: Bool
      }
  | OptionValueNumber
      { -- | The name of the value
        optionValueName :: T.Text,
        -- | The localized name of the value
        optionValueLocalizedName :: Maybe LocalizedText,
        -- | The description of the value
        optionValueDescription :: T.Text,
        -- | The localized description of the value
        optionValueLocalizedDescription :: Maybe LocalizedText,
        -- | Whether this option is required
        optionValueRequired :: Bool,
        -- | Whether to autocomplete or have a list of named choices. For neither option, use `Left False`
        optionValueNumberChoices :: AutocompleteOrChoice Number,
        -- | The lower bound of values permitted. If choices are provided or autocomplete is on, this can be ignored
        optionValueNumberMinVal :: Maybe Number,
        -- | The upper bound of values permitted. If choices are provided or autocomplete is on, this can be ignored
        optionValueNumberMaxVal :: Maybe Number
      }
  deriving (Show, Eq, Read)

instance FromJSON OptionValue where
  parseJSON =
    withObject
      "OptionValue"
      ( \v -> do
          name <- v .: "name"
          lname <- v .:? "name_localizations"
          desc <- v .: "description"
          ldesc <- v .:? "description_localizations"
          required <- v .:? "required" .!= False
          t <- v .: "type" :: Parser Int
          case t of
            3 ->
              OptionValueString name lname desc ldesc required
                <$> parseJSON (Object v)
                <*> v .:? "min_length"
                <*> v .:? "max_length"
            4 ->
              OptionValueInteger name lname desc ldesc required
                <$> parseJSON (Object v)
                <*> v .:? "min_value"
                <*> v .:? "max_value"
            10 ->
              OptionValueNumber name lname desc ldesc required
                <$> parseJSON (Object v)
                <*> v .:? "min_value"
                <*> v .:? "max_value"
            7 ->
              OptionValueChannel name lname desc ldesc required
                <$> v .:? "channel_types"
            5 -> return $ OptionValueBoolean name lname desc ldesc required
            6 -> return $ OptionValueUser name lname desc ldesc required
            8 -> return $ OptionValueRole name lname desc ldesc required
            9 -> return $ OptionValueMentionable name lname desc ldesc required
            _ -> fail "unknown application command option value type"
      )

instance ToJSON OptionValue where
  toJSON OptionValueString {..} =
    object
      [ ("type", Number 3),
        ("name", toJSON optionValueName),
        ("description", toJSON optionValueDescription),
        ("name_localizations", toJSON optionValueLocalizedName),
        ("description_localizations", toJSON optionValueLocalizedDescription),
        ("required", toJSON optionValueRequired),
        ("min_length", toJSON optionValueStringMinLen),
        ("max_length", toJSON optionValueStringMaxLen),
        choiceOrAutocompleteToJSON optionValueStringChoices
      ]
  toJSON OptionValueInteger {..} =
    object
      [ ("type", Number 4),
        ("name", toJSON optionValueName),
        ("description", toJSON optionValueDescription),
        ("name_localizations", toJSON optionValueLocalizedName),
        ("description_localizations", toJSON optionValueLocalizedDescription),
        ("required", toJSON optionValueRequired),
        ("min_value", toJSON optionValueIntegerMinVal),
        ("max_value", toJSON optionValueIntegerMaxVal),
        choiceOrAutocompleteToJSON optionValueIntegerChoices
      ]
  toJSON OptionValueNumber {..} =
    object
      [ ("type", Number 10),
        ("name", toJSON optionValueName),
        ("description", toJSON optionValueDescription),
        ("name_localizations", toJSON optionValueLocalizedName),
        ("description_localizations", toJSON optionValueLocalizedDescription),
        ("required", toJSON optionValueRequired),
        ("min_value", toJSON optionValueNumberMinVal),
        ("max_value", toJSON optionValueNumberMaxVal),
        choiceOrAutocompleteToJSON optionValueNumberChoices
      ]
  toJSON OptionValueChannel {..} =
    object
      [ ("type", Number 7),
        ("name", toJSON optionValueName),
        ("description", toJSON optionValueDescription),
        ("name_localizations", toJSON optionValueLocalizedName),
        ("description_localizations", toJSON optionValueLocalizedDescription),
        ("required", toJSON optionValueRequired),
        ("channel_types", toJSON optionValueChannelTypes)
      ]
  toJSON acov =
    object
      [ ("type", Number (t acov)),
        ("name", toJSON $ optionValueName acov),
        ("description", toJSON $ optionValueDescription acov),
        ("name_localizations", toJSON $ optionValueLocalizedName acov),
        ("description_localizations", toJSON $ optionValueLocalizedDescription acov),
        ("required", toJSON $ optionValueRequired acov)
      ]
    where
      t OptionValueBoolean {} = 5
      t OptionValueUser {} = 6
      t OptionValueRole {} = 8
      t OptionValueMentionable {} = 9
      t _ = -1

-- | Data type to be used when creating application commands. The specification
-- is below.
--
-- If a command of the same type and and name is sent to the server, it will
-- overwrite any command that already exists in the same scope (guild vs
-- global).
--
-- The description has to be empty for non-slash command application
-- commands, as do the options. The options need to be `Nothing` for non-slash
-- commands, too. If one of the options is a subcommand or subcommand group,
-- the base command will no longer be usable.
--
-- A subcommand group can have subcommands within it. This is the maximum amount
-- of command nesting permitted.
--
-- https://discord.com/developers/docs/interactions/application-commands#create-global-application-command
data CreateApplicationCommand
  = CreateApplicationCommandChatInput
      { -- | The application command name (1-32 chars).
        createName :: T.Text,
        -- | The localized application name
        createLocalizedName :: Maybe LocalizedText,
        -- | The application command description (1-100 chars).
        createDescription :: T.Text,
        -- | The localized application command description.
        createLocalizedDescription :: Maybe LocalizedText,
        -- | What options the application (max length 25).
        createOptions :: Maybe Options,
        -- | The default permissions required for members set when using the command
        -- in a guild.
        -- Set of permissions represented as a bit set.
        createDefaultMemberPermissions :: Maybe T.Text,
        -- | Whether the command is enabled by default when the application is added
        -- to a guild.
        createDefaultPermission :: Bool
      }
  | CreateApplicationCommandUser
      { -- | The application command name (1-32 chars).
        createName :: T.Text,
        -- | The localized application name
        createLocalizedName :: Maybe LocalizedText,
        -- | The default permissions required for members set when using the command
        -- in a guild.
        -- Set of permissions represented as a bit set.
        createDefaultMemberPermissions :: Maybe T.Text,
        -- | Whether the command is enabled by default when the application is added
        -- to a guild.
        createDefaultPermission :: Bool
      }
  | CreateApplicationCommandMessage
      { -- | The application command name (1-32 chars).
        createName :: T.Text,
        -- | The localized application name
        createLocalizedName :: Maybe LocalizedText,
        -- | The default permissions required for members set when using the command
        -- in a guild.
        -- Set of permissions represented as a bit set.
        createDefaultMemberPermissions :: Maybe T.Text,
        -- | Whether the command is enabled by default when the application is added
        -- to a guild.
        createDefaultPermission :: Bool
      }
  deriving (Show, Eq, Read)

instance ToJSON CreateApplicationCommand where
  toJSON CreateApplicationCommandChatInput {..} =
    objectFromMaybes
      [ "name" .== createName,
        "name_localizations" .=? createLocalizedName,
        "description" .== createDescription,
        "description_localizations" .=? createLocalizedDescription,
        "options" .=? createOptions,
        "default_member_permissions" .== createDefaultMemberPermissions,
        "default_permission" .== createDefaultPermission,
        "type" .== Number 1
      ]
  toJSON CreateApplicationCommandUser {..} =
    objectFromMaybes
      [ "name" .== createName,
        "name_localizations" .=? createLocalizedName,
        "default_member_permissions" .== createDefaultMemberPermissions,
        "default_permission" .== createDefaultPermission,
        "type" .== Number 2
      ]
  toJSON CreateApplicationCommandMessage {..} =
    objectFromMaybes
      [ "name" .== createName,
        "name_localizations" .=? createLocalizedName,
        "default_member_permissions" .== createDefaultMemberPermissions,
        "default_permission" .== createDefaultPermission,
        "type" .== Number 3
      ]

nameIsValid :: Bool -> T.Text -> Bool
nameIsValid isChatInput name = l >= 1 && l <= 32 && isChatInput <= T.all (`elem` validChars) name
  where
    l = T.length name
    validChars = '-' : ['a' .. 'z']

-- | Create the basics for a chat input (slash command). Use record overwriting
-- to enter the other values. The name needs to be all lower case letters, and
-- between 1 and 32 characters. The description has to be non-empty and less
-- than or equal to 100 characters.
createChatInput :: T.Text -> T.Text -> Maybe CreateApplicationCommand
createChatInput name desc
  | nameIsValid True name && not (T.null desc) && T.length desc <= 100 = Just $ CreateApplicationCommandChatInput name Nothing desc Nothing Nothing Nothing True
  | otherwise = Nothing

-- | Create the basics for a user command. Use record overwriting to enter the
-- other values. The name needs to be between 1 and 32 characters.
createUser :: T.Text -> Maybe CreateApplicationCommand
createUser name
  | nameIsValid False name = Just $ CreateApplicationCommandUser name Nothing Nothing True
  | otherwise = Nothing

-- | Create the basics for a message command. Use record overwriting to enter
-- the other values. The name needs to be between 1 and 32 characters.
createMessage :: T.Text -> Maybe CreateApplicationCommand
createMessage name
  | nameIsValid False name = Just $ CreateApplicationCommandMessage name Nothing Nothing True
  | otherwise = Nothing

-- | Data type to be used when editing application commands. The specification
-- is below. See `CreateApplicationCommand` for an explanation for the
-- parameters.
--
-- https://discord.com/developers/docs/interactions/application-commands#edit-global-application-command
data EditApplicationCommand
  = EditApplicationCommandChatInput
      { editName :: Maybe T.Text,
        editLocalizedName :: Maybe LocalizedText,
        editDescription :: Maybe T.Text,
        editLocalizedDescription :: Maybe LocalizedText,
        editOptions :: Maybe Options,
        editDefaultPermission :: Maybe Bool
      }
  | EditApplicationCommandUser
      { editName :: Maybe T.Text,
        editLocalizedName :: Maybe LocalizedText,
        editDefaultPermission :: Maybe Bool
      }
  | EditApplicationCommandMessage
      { editName :: Maybe T.Text,
        editLocalizedName :: Maybe LocalizedText,
        editDefaultPermission :: Maybe Bool
      }

defaultEditApplicationCommand :: Int -> EditApplicationCommand
defaultEditApplicationCommand 2 = EditApplicationCommandUser Nothing Nothing Nothing
defaultEditApplicationCommand 3 = EditApplicationCommandMessage Nothing Nothing Nothing
defaultEditApplicationCommand _ = EditApplicationCommandChatInput Nothing Nothing Nothing Nothing Nothing Nothing

instance ToJSON EditApplicationCommand where
  toJSON EditApplicationCommandChatInput {..} =
    objectFromMaybes
      [ "name" .=? editName,
        "name_localization" .=? editLocalizedName,
        "description" .=? editDescription,
        "description_localization" .=? editLocalizedDescription,
        "options" .=? editOptions,
        "default_permission" .=? editDefaultPermission,
        "type" .== Number 1
      ]
  toJSON EditApplicationCommandUser {..} =
    objectFromMaybes
      [ "name" .=? editName,
        "name_localization" .=? editLocalizedName,
        "default_permission" .=? editDefaultPermission,
        "type" .== Number 2
      ]
  toJSON EditApplicationCommandMessage {..} =
    objectFromMaybes
      [ "name" .=? editName,
        "name_localization" .=? editLocalizedName,
        "default_permission" .=? editDefaultPermission,
        "type" .== Number 3
      ]

data Choice a = Choice
  { -- | The name of the choice
    choiceName :: T.Text,
    -- | The localized name of the choice
    choiceLocalizedName :: Maybe LocalizedText,
    -- | The value of the choice
    choiceValue :: a
  }
  deriving (Show, Read, Eq, Ord)

instance Functor Choice where
  fmap f (Choice s l a) = Choice s l (f a)

instance (ToJSON a) => ToJSON (Choice a) where
  toJSON Choice {..} = object [
    ("name", toJSON choiceName),
    ("value", toJSON choiceValue),
    ("name_localizations", toJSON choiceLocalizedName)]

instance (FromJSON a) => FromJSON (Choice a) where
  parseJSON =
    withObject
      "Choice"
      ( \v ->
          Choice
            <$> v .: "name"
            <*> v .:? "name_localizations"
            <*> v .: "value"
      )

type AutocompleteOrChoice a = Either Bool [Choice a]

instance {-# OVERLAPPING #-} (FromJSON a) => FromJSON (AutocompleteOrChoice a) where
  parseJSON =
    withObject
      "AutocompleteOrChoice"
      ( \v -> do
          mcs <- v .:! "choices"
          case mcs of
            Nothing -> Left <$> v .:? "autocomplete" .!= False
            Just cs -> return $ Right cs
      )

choiceOrAutocompleteToJSON :: (ToJSON a) => AutocompleteOrChoice a -> Pair
choiceOrAutocompleteToJSON (Left b) = ("autocomplete", toJSON b)
choiceOrAutocompleteToJSON (Right cs) = ("choices", toJSON cs)

-- | The different channel types.
--
-- https://discord.com/developers/docs/resources/channel#channel-object-channel-types
data ApplicationCommandChannelType
  = -- | A text channel in a server.
    ApplicationCommandChannelTypeGuildText
  | -- | A direct message between users.
    ApplicationCommandChannelTypeDM
  | -- | A voice channel in a server.
    ApplicationCommandChannelTypeGuildVoice
  | -- | A direct message between multiple users.
    ApplicationCommandChannelTypeGroupDM
  | -- | An organizational category that contains up to 50 channels.
    ApplicationCommandChannelTypeGuildCategory
  | -- | A channel that users can follow and crosspost into their own server.
    ApplicationCommandChannelTypeGuildNews
  | -- | A channel in which game developers can sell their game on discord.
    ApplicationCommandChannelTypeGuildStore
  | -- | A temporary sub-channel within a guild_news channel.
    ApplicationCommandChannelTypeGuildNewsThread
  | -- | A temporary sub-channel within a guild_text channel.
    ApplicationCommandChannelTypeGuildPublicThread
  | -- | A temporary sub-channel within a GUILD_TEXT channel that is only
    -- viewable by those invited and those with the MANAGE_THREADS permission
    ApplicationCommandChannelTypeGuildPrivateThread
  | -- | A voice channel for hosting events with an audience.
    ApplicationCommandChannelTypeGuildStageVoice
  deriving (Show, Read, Data, Eq)

instance InternalDiscordEnum ApplicationCommandChannelType where
  discordTypeStartValue = ApplicationCommandChannelTypeGuildText
  fromDiscordType ApplicationCommandChannelTypeGuildText = 0
  fromDiscordType ApplicationCommandChannelTypeDM = 1
  fromDiscordType ApplicationCommandChannelTypeGuildVoice = 2
  fromDiscordType ApplicationCommandChannelTypeGroupDM = 3
  fromDiscordType ApplicationCommandChannelTypeGuildCategory = 4
  fromDiscordType ApplicationCommandChannelTypeGuildNews = 5
  fromDiscordType ApplicationCommandChannelTypeGuildStore = 6
  fromDiscordType ApplicationCommandChannelTypeGuildNewsThread = 10
  fromDiscordType ApplicationCommandChannelTypeGuildPublicThread = 11
  fromDiscordType ApplicationCommandChannelTypeGuildPrivateThread = 12
  fromDiscordType ApplicationCommandChannelTypeGuildStageVoice = 13

instance ToJSON ApplicationCommandChannelType where
  toJSON = toJSON . fromDiscordType

instance FromJSON ApplicationCommandChannelType where
  parseJSON = discordTypeParseJSON "ApplicationCommandChannelType"

data GuildApplicationCommandPermissions = GuildApplicationCommandPermissions
  { -- | The id of the command.
    guildApplicationCommandPermissionsId :: ApplicationCommandId,
    -- | The id of the application.
    guildApplicationCommandPermissionsApplicationId :: ApplicationId,
    -- | The id of the guild.
    guildApplicationCommandPermissionsGuildId :: GuildId,
    -- | The permissions for the command in the guild.
    guildApplicationCommandPermissionsPermissions :: [ApplicationCommandPermissions]
  }
  deriving (Show, Read, Eq, Ord)

instance FromJSON GuildApplicationCommandPermissions where
  parseJSON =
    withObject
      "GuildApplicationCommandPermissions"
      ( \v ->
          GuildApplicationCommandPermissions
            <$> v .: "id"
            <*> v .: "application_id"
            <*> v .: "guild_id"
            <*> v .: "permissions"
      )

instance ToJSON GuildApplicationCommandPermissions where
  toJSON GuildApplicationCommandPermissions {..} =
    objectFromMaybes
      [ "id" .== guildApplicationCommandPermissionsId,
        "application_id" .== guildApplicationCommandPermissionsApplicationId,
        "guild_id" .== guildApplicationCommandPermissionsGuildId,
        "permissions" .== guildApplicationCommandPermissionsPermissions
      ]

-- | Application command permissions allow you to enable or disable commands for
-- specific users or roles within a guild.
data ApplicationCommandPermissions = ApplicationCommandPermissions
  { -- | The id of the role or user.
    applicationCommandPermissionsId :: Snowflake,
    -- | Choose either role (1) or user (2).
    applicationCommandPermissionsType :: Integer,
    -- | Whether to allow or not.
    applicationCommandPermissionsPermission :: Bool
  }
  deriving (Show, Read, Eq, Ord)

instance FromJSON ApplicationCommandPermissions where
  parseJSON =
    withObject
      "ApplicationCommandPermissions"
      ( \v ->
          ApplicationCommandPermissions
            <$> v .: "id"
            <*> v .: "type"
            <*> v .: "permission"
      )

instance ToJSON ApplicationCommandPermissions where
  toJSON ApplicationCommandPermissions {..} =
    objectFromMaybes
      [ "id" .== applicationCommandPermissionsId,
        "type" .== applicationCommandPermissionsType,
        "permission" .== applicationCommandPermissionsPermission
      ]

type Locale = T.Text

type TextTranslation = (Locale, T.Text)

newtype LocalizedText = LocalizedText [TextTranslation] deriving (Show, Read, Eq, Ord)

instance ToJSON LocalizedText where
  toJSON (LocalizedText lt) = object . map (second toJSON) $ lt
  
instance FromJSON LocalizedText where
  parseJSON = withObject "LocalizedText"
    (\o -> do
        translations <- sequenceSecond
                        . map (second parseJSON)
                        . objectToList
                        $ o
        return $ LocalizedText translations
    ) where
      sequenceSecond :: Monad m => [(a, m b)] -> m [(a, b)]
      sequenceSecond l = fmap (zip a) . sequence $ b 
        where
          (a, b) = unzip l
