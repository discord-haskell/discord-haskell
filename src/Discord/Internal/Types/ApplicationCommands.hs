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
    StringNumber (..),
    IntDouble (..),
  )
where

import Control.Applicative (Alternative ((<|>)))
import Data.Aeson
import Data.Data (Data (dataTypeOf), dataTypeConstrs, fromConstr)
import Data.Default (Default (..))
import Data.Maybe (fromJust)
import Data.Text (unpack)
import Discord.Internal.Types.Prelude (ApplicationId, GuildId, Snowflake)

toMaybeJSON :: (ToJSON a) => a -> Maybe Value
toMaybeJSON = return . toJSON

makeTable :: (Data t, Enum t) => t -> [(Int, t)]
makeTable t = map (\cData -> let c = fromConstr cData in (fromEnum c, c)) (dataTypeConstrs $ dataTypeOf t)

-- | What type of application command. Represents slash commands, right clicking
-- a user, and right clicking a message respectively.
data ApplicationCommandType
  = -- | Slash commands
    ACTCHAT_INPUT
  | -- | User commands
    ACTUSER
  | -- | Message commands
    ACTMESSAGE
  deriving (Show, Read, Data, Eq)

instance Enum ApplicationCommandType where
  fromEnum ACTCHAT_INPUT = 1
  fromEnum ACTUSER = 2
  fromEnum ACTMESSAGE = 3
  toEnum a = fromJust $ lookup a table
    where
      table = makeTable ACTCHAT_INPUT

instance ToJSON ApplicationCommandType where
  toJSON = toJSON . fromEnum

instance FromJSON ApplicationCommandType where
  parseJSON = withScientific "ApplicationCommandType" (return . toEnum . round)

type ApplicationCommandId = Snowflake

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
    cApplicationCommandName :: String,
    -- | The application command description (1-100 chars). Has to be empty for
    -- non-slash commands.
    cApplicationCommandDescription :: String,
    -- | What options the application (max length 25). Has to be `Nothing` for
    -- non-slash  commands.
    cApplicationCommandOptions :: Maybe [ApplicationCommandOption],
    -- | Whether the command is enabled by default when the application is added
    -- to a guild. Defaults to true if not present
    cApplicationCommandDefaultPermission :: Maybe Bool,
    -- | What the type of the command is. If `Nothing`, defaults to slash
    -- commands.
    cApplicationCommandType :: Maybe ApplicationCommandType
  }
  deriving (Show, Eq, Read)

instance Default CreateApplicationCommand where
  def = CreateApplicationCommand "createappcom" "createappcom desc" Nothing Nothing Nothing

instance ToJSON CreateApplicationCommand where
  toJSON CreateApplicationCommand {..} =
    object
      [ (name, value)
        | (name, Just value) <-
            [ ("name", toMaybeJSON cApplicationCommandName),
              ("description", toMaybeJSON cApplicationCommandDescription),
              ("options", toJSON <$> cApplicationCommandOptions),
              ("default_permission", toJSON <$> cApplicationCommandDefaultPermission),
              ("type", toJSON <$> cApplicationCommandType)
            ]
      ]

-- | Data type to be used when editing application commands. The specification
-- is below. See `CreateApplicationCommand` for an explanation for the
-- parameters.
--
-- https://discord.com/developers/docs/interactions/application-commands#edit-global-application-command
data EditApplicationCommand = EditApplicationCommand
  { eApplicationCommandName :: Maybe String,
    eApplicationCommandDescription :: Maybe String,
    eApplicationCommandOptions :: Maybe [ApplicationCommandOption],
    eApplicationCommandDefaultPermission :: Maybe Bool,
    eApplicationCommandType :: Maybe ApplicationCommandType
  }

instance Default EditApplicationCommand where
  def = EditApplicationCommand Nothing Nothing Nothing Nothing Nothing

instance ToJSON EditApplicationCommand where
  toJSON EditApplicationCommand {..} =
    object
      [ (name, value)
        | (name, Just value) <-
            [ ("name", toJSON <$> eApplicationCommandName),
              ("description", toJSON <$> eApplicationCommandDescription),
              ("options", toJSON <$> eApplicationCommandOptions),
              ("default_permission", toJSON <$> eApplicationCommandDefaultPermission),
              ("type", toJSON <$> eApplicationCommandType)
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
    -- | Unique id of the parent application.
    applicationCommandApplicationId :: ApplicationId,
    -- | The guild id of the command if not global.
    applicationCommandGuildId :: Maybe GuildId,
    -- | Must be 1-32 characters.
    applicationCommandName :: String,
    -- | Must be empty for USER and MESSAGE commands, otherwise 1-100 chars.
    applicationCommandDescription :: String,
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
    optionType :: ApplicationCommandOptionType,
    -- | The name of the option . 1-32 characters
    optionName :: String,
    -- | 1-100 characters
    optionDescription :: String,
    -- | Is the parameter required? default false
    optionRequired :: Maybe Bool,
    -- | If specified, these are the only valid options to choose from. Type
    -- depends on optionType, and can only be specified for STRING, INTEGER or
    -- NUMBER types.
    optionChoices :: Maybe [ApplicationCommandOptionChoice],
    -- | If the option type is a subcommand or subcommand group type, these are
    -- the parameters to the subcommand.
    optionOptions :: Maybe [ApplicationCommandOption],
    -- | If option is channel type, these are the only channel types allowed.
    optionChannelTypes :: Maybe [ApplicationCommandChannelType],
    -- | If option is number type, minimum value for the number
    optionMinVal :: Maybe IntDouble,
    -- | if option is number type, maximum value for the number
    optionMaxVal :: Maybe IntDouble,
    -- | Enable auto complete interactions. may not be set to true if choices is present.
    optionAutocomplete :: Maybe Bool
  }
  deriving (Show, Eq, Read)

instance Default ApplicationCommandOption where
  def = ApplicationCommandOption STRING "appcomop" "appcomop desc" Nothing Nothing Nothing Nothing Nothing Nothing Nothing

instance ToJSON ApplicationCommandOption where
  toJSON ApplicationCommandOption {..} =
    object
      [ (name, value)
        | (name, Just value) <-
            [ ("type", toMaybeJSON optionType),
              ("name", toMaybeJSON optionName),
              ("description", toMaybeJSON optionDescription),
              ("required", toJSON <$> optionRequired),
              ("choices", toJSON <$> optionChoices),
              ("options", toJSON <$> optionOptions),
              ("channel_types", toJSON <$> optionChannelTypes),
              ("min_val", toJSON <$> optionMinVal),
              ("max_val", toJSON <$> optionMaxVal),
              ("autocomplete", toJSON <$> optionAutocomplete)
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
    SUB_COMMAND
  | -- | A subcommand group. It can take further options, excluding sub command
    -- groups.
    SUB_COMMAND_GROUP
  | -- | Can typically be provided with default values.
    STRING
  | -- | Can typically be provided with default values, and possibly with
    -- minimum and maximum values.
    INTEGER
  | BOOLEAN
  | USER
  | -- | Can be limited in the types of the channel allowed.
    CHANNEL
  | ROLE
  | -- | Users and roles.
    MENTIONABLE
  | -- | Can typically be provided with default values, and possibly with
    -- minimum and maximum values. Represents a double.
    NUMBER
  deriving (Show, Read, Data, Eq)

instance Enum ApplicationCommandOptionType where
  fromEnum SUB_COMMAND = 1
  fromEnum SUB_COMMAND_GROUP = 2
  fromEnum STRING = 3
  fromEnum INTEGER = 4
  fromEnum BOOLEAN = 5
  fromEnum USER = 6
  fromEnum CHANNEL = 7
  fromEnum ROLE = 8
  fromEnum MENTIONABLE = 9
  fromEnum NUMBER = 10
  toEnum a = fromJust $ lookup a table
    where
      table = makeTable SUB_COMMAND

instance ToJSON ApplicationCommandOptionType where
  toJSON = toJSON . fromEnum

instance FromJSON ApplicationCommandOptionType where
  parseJSON = withScientific "ApplicationCommandOptionType" (return . toEnum . round)

-- | Utility data type to store integers or doubles.
data IntDouble = IDI Int | IDD Double
  deriving (Show, Read, Eq)

instance ToJSON IntDouble where
  toJSON (IDI s) = toJSON s
  toJSON (IDD i) = toJSON i

instance FromJSON IntDouble where
  parseJSON v = (IDI <$> parseJSON v) <|> (IDD <$> parseJSON v)

-- | Utility data type to store strings or number types.
data StringNumber = SNS String | SNN IntDouble
  deriving (Show, Read, Eq)

instance ToJSON StringNumber where
  toJSON (SNS s) = toJSON s
  toJSON (SNN i) = toJSON i

instance FromJSON StringNumber where
  parseJSON (String t) = return $ SNS $ unpack t
  parseJSON v = SNN <$> parseJSON v

-- | The choices for a particular option.
data ApplicationCommandOptionChoice = ApplicationCommandOptionChoice
  { choiceName :: String,
    choiceValue :: StringNumber
  }
  deriving (Show, Read, Eq)

instance ToJSON ApplicationCommandOptionChoice where
  toJSON ApplicationCommandOptionChoice {..} = object [("name", toJSON choiceName), ("value", toJSON choiceValue)]

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
    GUILD_TEXT
  | -- | A direct message between users.
    DM
  | -- | A voice channel in a server.
    GUILD_VOICE
  | -- | A direct message between multiple users.
    GROUP_DM
  | -- | An organizational category that contains up to 50 channels.
    GUILD_CATEGORY
  | -- | A channel that users can follow and crosspost into their own server.
    GUILD_NEWS
  | -- | A channel in which game developers can sell their game on discord.
    GUILD_STORE
  | -- | A temporary sub-channel within a guild_news channel.
    GUILD_NEWS_THREAD
  | -- | A temporary sub-channel within a guild_text channel
    GUILD_PUBLIC_THREAD
  | -- | A temporary sub-channel within a GUILD_TEXT channel that is only
    -- viewable by those invited and those with the MANAGE_THREADS permission
    GUILD_PRIVATE_THREAD
  | -- | A voice channel for hosting events with an audience.
    GUILD_STAGE_VOICE
  deriving (Show, Read, Data, Eq)

instance Enum ApplicationCommandChannelType where
  fromEnum GUILD_TEXT = 0
  fromEnum DM = 1
  fromEnum GUILD_VOICE = 2
  fromEnum GROUP_DM = 3
  fromEnum GUILD_CATEGORY = 4
  fromEnum GUILD_NEWS = 5
  fromEnum GUILD_STORE = 6
  fromEnum GUILD_NEWS_THREAD = 10
  fromEnum GUILD_PUBLIC_THREAD = 11
  fromEnum GUILD_PRIVATE_THREAD = 12
  fromEnum GUILD_STAGE_VOICE = 13
  toEnum a = fromJust $ lookup a table
    where
      table = makeTable GUILD_TEXT

instance ToJSON ApplicationCommandChannelType where
  toJSON = toJSON . fromEnum

instance FromJSON ApplicationCommandChannelType where
  parseJSON = withScientific "ApplicationCommandChannelType" (return . toEnum . round)
