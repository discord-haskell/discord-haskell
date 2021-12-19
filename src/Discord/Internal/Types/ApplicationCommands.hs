{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Discord.Internal.Types.ApplicationCommands
  ( ApplicationCommand (..),
    ApplicationCommandId,
    CreateApplicationCommand (..),
    EditApplicationCommand (..),
    ApplicationCommandType (..),
    ApplicationCommandOption (..),
    ApplicationCommandOptionType (..),
    ApplicationCommandOptionChoice (..),
    ApplicationCommandChannelType (..),
    GuildApplicationCommandPermissions (..),
    ApplicationCommandPermissions (..),
    ApplicationCommandPermissionType (..),
    StringNumberValue (..),
  )
where

import Data.Aeson
import Data.Data (Data)
import Data.Default (Default (..))
import Data.Maybe (fromJust)
import Data.Scientific (Scientific)
import qualified Data.Text as T
import Discord.Internal.Types.Prelude (ApplicationCommandId, ApplicationId, GuildId, Snowflake, makeTable, toMaybeJSON)

-- | What type of application command. Represents slash commands, right clicking
-- a user, and right clicking a message respectively.
data ApplicationCommandType
  = -- | Slash commands
    ApplicationCommandTypeChatInput
  | -- | User commands
    ApplicationCommandTypeUser
  | -- | Message commands
    ApplicationCommandTypeMessage
  deriving (Show, Read, Data, Eq)

instance Enum ApplicationCommandType where
  fromEnum ApplicationCommandTypeChatInput = 1
  fromEnum ApplicationCommandTypeUser = 2
  fromEnum ApplicationCommandTypeMessage = 3
  toEnum a = fromJust $ lookup a table
    where
      table = makeTable ApplicationCommandTypeChatInput

instance ToJSON ApplicationCommandType where
  toJSON = toJSON . fromEnum

instance FromJSON ApplicationCommandType where
  parseJSON = withScientific "ApplicationCommandType" (return . toEnum . round)

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
data CreateApplicationCommand = CreateApplicationCommand
  { -- | The application command name (1-32 chars).
    createApplicationCommandName :: T.Text,
    -- | The application command description (1-100 chars). Has to be empty for
    -- non-slash commands.
    createApplicationCommandDescription :: T.Text,
    -- | What options the application (max length 25). Has to be `Nothing` for
    -- non-slash  commands.
    createApplicationCommandOptions :: Maybe [ApplicationCommandOption],
    -- | Whether the command is enabled by default when the application is added
    -- to a guild. Defaults to true if not present
    createApplicationCommandDefaultPermission :: Maybe Bool,
    -- | What the type of the command is. If `Nothing`, defaults to slash
    -- commands.
    createApplicationCommandType :: Maybe ApplicationCommandType
  }
  deriving (Show, Eq, Read)

instance Default CreateApplicationCommand where
  def = CreateApplicationCommand "createappcom" "createappcom desc" Nothing Nothing Nothing

instance ToJSON CreateApplicationCommand where
  toJSON CreateApplicationCommand {..} =
    object
      [ (name, value)
        | (name, Just value) <-
            [ ("name", toMaybeJSON createApplicationCommandName),
              ("description", toMaybeJSON createApplicationCommandDescription),
              ("options", toJSON <$> createApplicationCommandOptions),
              ("default_permission", toJSON <$> createApplicationCommandDefaultPermission),
              ("type", toJSON <$> createApplicationCommandType)
            ]
      ]

-- | Data type to be used when editing application commands. The specification
-- is below. See `CreateApplicationCommand` for an explanation for the
-- parameters.
--
-- https://discord.com/developers/docs/interactions/application-commands#edit-global-application-command
data EditApplicationCommand = EditApplicationCommand
  { editApplicationCommandName :: Maybe T.Text,
    editApplicationCommandDescription :: Maybe T.Text,
    editApplicationCommandOptions :: Maybe [ApplicationCommandOption],
    editApplicationCommandDefaultPermission :: Maybe Bool,
    editApplicationCommandType :: Maybe ApplicationCommandType
  }

instance Default EditApplicationCommand where
  def = EditApplicationCommand Nothing Nothing Nothing Nothing Nothing

instance ToJSON EditApplicationCommand where
  toJSON EditApplicationCommand {..} =
    object
      [ (name, value)
        | (name, Just value) <-
            [ ("name", toJSON <$> editApplicationCommandName),
              ("description", toJSON <$> editApplicationCommandDescription),
              ("options", toJSON <$> editApplicationCommandOptions),
              ("default_permission", toJSON <$> editApplicationCommandDefaultPermission),
              ("type", toJSON <$> editApplicationCommandType)
            ]
      ]

-- | The full information about an application command, obtainable with the
-- various get requests. In theory, you never need to construct one of these -
-- so if you are, reconsider what you're doing.
--
-- https://discord.com/developers/docs/interactions/application-commands#application-command-object-application-command-structure
data ApplicationCommand = ApplicationCommand
  { -- | Unique id of the command.
    applicationCommandId :: ApplicationCommandId,
    -- | The type of the command.
    applicationCommandType :: Maybe ApplicationCommandType,
    -- | Unique id of the parent application (the bot).
    applicationCommandApplicationId :: ApplicationId,
    -- | The guild id of the command if not global.
    applicationCommandGuildId :: Maybe GuildId,
    -- | Must be 1-32 characters.
    applicationCommandName :: T.Text,
    -- | Must be empty for USER and MESSAGE commands, otherwise 1-100 chars.
    applicationCommandDescription :: T.Text,
    -- | CHAT_INPUT only, parameters to command
    applicationCommandOptions :: Maybe [ApplicationCommandOption],
    -- | whether the command is enabled by default when the app is added to a
    -- guild. Defaults to true.
    applicationCommandDefaultPermission :: Maybe Bool,
    applicationCommandVersion :: Snowflake
  }
  deriving (Show)

instance FromJSON ApplicationCommand where
  parseJSON =
    withObject
      "ApplicationCommand"
      ( \v ->
          ApplicationCommand
            <$> v .: "id"
            <*> v .:? "type"
            <*> v .: "application_id"
            <*> v .:? "guild_id"
            <*> v .: "name"
            <*> v .: "description"
            <*> v .:? "options"
            <*> v .:? "default_permission"
            <*> v .: "version"
      )

-- | This is the structure that designates different options for slash commands.
--
-- https://discord.com/developers/docs/interactions/application-commands#application-command-object-application-command-option-structure
data ApplicationCommandOption = ApplicationCommandOption
  { -- | What the type of this option is.
    applicationCommandOptionType :: ApplicationCommandOptionType,
    -- | The name of the option . 1-32 characters
    applicationCommandOptionName :: T.Text,
    -- | 1-100 characters
    applicationCommandOptionDescription :: T.Text,
    -- | Is the parameter required? default false
    applicationCommandOptionRequired :: Maybe Bool,
    -- | If specified, these are the only valid options to choose from. Type
    -- depends on optionType, and can only be specified for STRING, INTEGER or
    -- NUMBER types.
    applicationCommandOptionChoices :: Maybe [ApplicationCommandOptionChoice],
    -- | If the option type is a subcommand or subcommand group type, these are
    -- the parameters to the subcommand.
    applicationCommandOptionOptions :: Maybe [ApplicationCommandOption],
    -- | If option is channel type, these are the only channel types allowed.
    applicationCommandOptionChannelTypes :: Maybe [ApplicationCommandChannelType],
    -- | If option is number type, minimum value for the number
    applicationCommandOptionMinVal :: Maybe Scientific,
    -- | if option is number type, maximum value for the number
    applicationCommandOptionMaxVal :: Maybe Scientific,
    -- | Enable auto complete interactions. may not be set to true if choices is present.
    applicationCommandOptionAutocomplete :: Maybe Bool
  }
  deriving (Show, Eq, Read)

instance Default ApplicationCommandOption where
  def = ApplicationCommandOption ApplicationCommandOptionTypeString "appcomop" "appcomop desc" Nothing Nothing Nothing Nothing Nothing Nothing Nothing

instance ToJSON ApplicationCommandOption where
  toJSON ApplicationCommandOption {..} =
    object
      [ (name, value)
        | (name, Just value) <-
            [ ("type", toMaybeJSON applicationCommandOptionType),
              ("name", toMaybeJSON applicationCommandOptionName),
              ("description", toMaybeJSON applicationCommandOptionDescription),
              ("required", toJSON <$> applicationCommandOptionRequired),
              ("choices", toJSON <$> applicationCommandOptionChoices),
              ("options", toJSON <$> applicationCommandOptionOptions),
              ("channel_types", toJSON <$> applicationCommandOptionChannelTypes),
              ("min_val", toJSON <$> applicationCommandOptionMinVal),
              ("max_val", toJSON <$> applicationCommandOptionMaxVal),
              ("autocomplete", toJSON <$> applicationCommandOptionAutocomplete)
            ]
      ]

instance FromJSON ApplicationCommandOption where
  parseJSON =
    withObject
      "ApplicationCommandOption"
      ( \v ->
          ApplicationCommandOption
            <$> v .: "type"
            <*> v .: "name"
            <*> v .: "description"
            <*> v .:? "required"
            <*> v .:? "choices"
            <*> v .:? "options"
            <*> v .:? "channel_types"
            <*> v .:? "min_val"
            <*> v .:? "max_val"
            <*> v .:? "autocomplete"
      )

-- | What type of command option. Can represent a wide variety of types, so
-- please check out the documentation below.
--
-- https://discord.com/developers/docs/interactions/application-commands#application-command-object-application-command-option-type
data ApplicationCommandOptionType
  = -- | A subcommand. It can take further options, excluding sub commands and
    -- sub command groups.
    ApplicationCommandOptionTypeSubCommand
  | -- | A subcommand group. It can take further options, excluding sub command
    -- groups.
    ApplicationCommandOptionTypeSubCommandGroup
  | -- | Can typically be provided with default values.
    ApplicationCommandOptionTypeString
  | -- | Can typically be provided with default values, and possibly with
    -- minimum and maximum values.
    ApplicationCommandOptionTypeInteger
  | ApplicationCommandOptionTypeBoolean
  | ApplicationCommandOptionTypeUser
  | -- | Can be limited in the types of the channel allowed.
    ApplicationCommandOptionTypeChannel
  | ApplicationCommandOptionTypeRole
  | -- | Users and roles.
    ApplicationCommandOptionTypeMentionable
  | -- | Can typically be provided with default values, and possibly with
    -- minimum and maximum values. Represents a double.
    ApplicationCommandOptionTypeNumber
  deriving (Show, Read, Data, Eq)

instance Enum ApplicationCommandOptionType where
  fromEnum ApplicationCommandOptionTypeSubCommand = 1
  fromEnum ApplicationCommandOptionTypeSubCommandGroup = 2
  fromEnum ApplicationCommandOptionTypeString = 3
  fromEnum ApplicationCommandOptionTypeInteger = 4
  fromEnum ApplicationCommandOptionTypeBoolean = 5
  fromEnum ApplicationCommandOptionTypeUser = 6
  fromEnum ApplicationCommandOptionTypeChannel = 7
  fromEnum ApplicationCommandOptionTypeRole = 8
  fromEnum ApplicationCommandOptionTypeMentionable = 9
  fromEnum ApplicationCommandOptionTypeNumber = 10
  toEnum a = fromJust $ lookup a table
    where
      table = makeTable ApplicationCommandOptionTypeSubCommand

instance ToJSON ApplicationCommandOptionType where
  toJSON = toJSON . fromEnum

instance FromJSON ApplicationCommandOptionType where
  parseJSON = withScientific "ApplicationCommandOptionType" (return . toEnum . round)

-- | Utility data type to store strings or number types.
data StringNumberValue = StringNumberValueString T.Text | StringNumberValueNumber Scientific
  deriving (Show, Read, Eq)

instance ToJSON StringNumberValue where
  toJSON (StringNumberValueString s) = toJSON s
  toJSON (StringNumberValueNumber i) = toJSON i

instance FromJSON StringNumberValue where
  parseJSON (String t) = return $ StringNumberValueString t
  parseJSON v = StringNumberValueNumber <$> parseJSON v

-- | The choices for a particular option.
data ApplicationCommandOptionChoice = ApplicationCommandOptionChoice
  { applicationCommandOptionChoiceName :: T.Text,
    applicationCommandOptionChoiceValue :: StringNumberValue
  }
  deriving (Show, Read, Eq)

instance ToJSON ApplicationCommandOptionChoice where
  toJSON ApplicationCommandOptionChoice {..} = object [("name", toJSON applicationCommandOptionChoiceName), ("value", toJSON applicationCommandOptionChoiceValue)]

instance FromJSON ApplicationCommandOptionChoice where
  parseJSON =
    withObject
      "ApplicationCommandOptionChoice"
      ( \v ->
          ApplicationCommandOptionChoice
            <$> v .: "name"
            <*> v .: "value"
      )

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
  | -- | A temporary sub-channel within a guild_text channel
    ApplicationCommandChannelTypeGuildPublicThread
  | -- | A temporary sub-channel within a GUILD_TEXT channel that is only
    -- viewable by those invited and those with the MANAGE_THREADS permission
    ApplicationCommandChannelTypeGuildPrivateThread
  | -- | A voice channel for hosting events with an audience.
    ApplicationCommandChannelTypeGuildStageVoice
  deriving (Show, Read, Data, Eq)

instance Enum ApplicationCommandChannelType where
  fromEnum ApplicationCommandChannelTypeGuildText = 0
  fromEnum ApplicationCommandChannelTypeDM = 1
  fromEnum ApplicationCommandChannelTypeGuildVoice = 2
  fromEnum ApplicationCommandChannelTypeGroupDM = 3
  fromEnum ApplicationCommandChannelTypeGuildCategory = 4
  fromEnum ApplicationCommandChannelTypeGuildNews = 5
  fromEnum ApplicationCommandChannelTypeGuildStore = 6
  fromEnum ApplicationCommandChannelTypeGuildNewsThread = 10
  fromEnum ApplicationCommandChannelTypeGuildPublicThread = 11
  fromEnum ApplicationCommandChannelTypeGuildPrivateThread = 12
  fromEnum ApplicationCommandChannelTypeGuildStageVoice = 13
  toEnum a = fromJust $ lookup a table
    where
      table = makeTable ApplicationCommandChannelTypeGuildText

instance ToJSON ApplicationCommandChannelType where
  toJSON = toJSON . fromEnum

instance FromJSON ApplicationCommandChannelType where
  parseJSON = withScientific "ApplicationCommandChannelType" (return . toEnum . round)

data GuildApplicationCommandPermissions = GuildApplicationCommandPermissions
  { -- | The id of the command
    guildApplicationCommandPermissionsId :: ApplicationCommandId,
    -- | The id of the application
    guildApplicationCommandPermissionsApplicationId :: ApplicationId,
    -- | The id of the guild
    guildApplicationCommandPermissionsGuildId :: GuildId,
    -- | The permissions for the command in the guild
    guildApplicationCommandPermissionsPermissions :: [ApplicationCommandPermissions]
  }
  deriving (Show, Eq, Ord, Read)

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
    object
      [ (name, value)
        | (name, Just value) <-
            [ ("id", toMaybeJSON guildApplicationCommandPermissionsId),
              ("application_id", toMaybeJSON guildApplicationCommandPermissionsApplicationId),
              ("guild_id", toMaybeJSON guildApplicationCommandPermissionsGuildId),
              ("permissions", toMaybeJSON guildApplicationCommandPermissionsPermissions)
            ]
      ]

data ApplicationCommandPermissions = ApplicationCommandPermissions
  { -- | The id of the role or user
    applicationCommandPermissionsId :: Snowflake,
    -- | Choose either role or user
    applicationCommandPermissionsType :: ApplicationCommandPermissionType,
    -- | Whether to allow or not
    applicationCommandPermissionsPermission :: Bool
  }
  deriving (Show, Eq, Ord, Read)

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
    object
      [ (name, value)
        | (name, Just value) <-
            [ ("id", toMaybeJSON applicationCommandPermissionsId),
              ("type", toMaybeJSON applicationCommandPermissionsType),
              ("permission", toMaybeJSON applicationCommandPermissionsPermission)
            ]
      ]

data ApplicationCommandPermissionType
  = ApplicationCommandPermissionTypeRole
  | ApplicationCommandPermissionTypeUser
  deriving (Show, Eq, Ord, Read, Data)

instance Enum ApplicationCommandPermissionType where
  fromEnum ApplicationCommandPermissionTypeRole = 1
  fromEnum ApplicationCommandPermissionTypeUser = 2
  toEnum a = fromJust $ lookup a table
    where
      table = makeTable ApplicationCommandPermissionTypeRole

instance ToJSON ApplicationCommandPermissionType where
  toJSON = toJSON . fromEnum

instance FromJSON ApplicationCommandPermissionType where
  parseJSON = withScientific "ApplicationCommandPermissionType" (return . toEnum . round)
