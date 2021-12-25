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
    ApplicationCommandOptionSubcommandOrGroup (..),
    ApplicationCommandOptionSubcommand (..),
    ApplicationCommandOptionValue (..),
    InternalApplicationCommand (..),
    CreateApplicationCommand (..),
    createApplicationCommandChatInput,
    createApplicationCommandUser,
    createApplicationCommandMessage,
    EditApplicationCommand (..),
    ApplicationCommandType (..),
    InternalApplicationCommandOption (..),
    ApplicationCommandOptionType (..),
    InternalApplicationCommandOptionChoice,
    Choice (..),
    ApplicationCommandChannelType (..),
    GuildApplicationCommandPermissions (..),
    ApplicationCommandPermissions (..),
    ApplicationCommandPermissionType (..),
    StringNumberValue (..),
  )
where

import Control.Applicative
import Data.Aeson
import Data.Char (isLower)
import Data.Data (Data)
import Data.Default (Default (..))
import Data.Maybe (fromJust, fromMaybe)
import Data.Scientific (Scientific)
import qualified Data.Text as T
import Discord.Internal.Types.Prelude (ApplicationCommandId, ApplicationId, GuildId, Internals (..), Snowflake, makeTable, toMaybeJSON)

data ApplicationCommand
  = ApplicationCommandUser
      { applicationCommandId :: ApplicationCommandId,
        applicationCommandApplicationId :: ApplicationId,
        applicationCommandGuildId :: Maybe GuildId,
        applicationCommandName :: T.Text,
        applicationCommandDefaultPermission :: Maybe Bool,
        applicationCommandVersion :: Snowflake
      }
  | ApplicationCommandMessage
      { applicationCommandId :: ApplicationCommandId,
        applicationCommandApplicationId :: ApplicationId,
        applicationCommandGuildId :: Maybe GuildId,
        applicationCommandName :: T.Text,
        applicationCommandDefaultPermission :: Maybe Bool,
        applicationCommandVersion :: Snowflake
      }
  | ApplicationCommandChatInput
      { applicationCommandId :: ApplicationCommandId,
        applicationCommandApplicationId :: ApplicationId,
        applicationCommandGuildId :: Maybe GuildId,
        applicationCommandName :: T.Text,
        applicationCommandDescription :: T.Text,
        applicationCommandOptions :: Maybe ApplicationCommandOptions,
        applicationCommandDefaultPermission :: Maybe Bool,
        applicationCommandVersion :: Snowflake
      }
  | ApplicationCommandUnknown InternalApplicationCommand
  deriving (Show, Eq, Read)

data ApplicationCommandOptions
  = ApplicationCommandOptionsSubcommands [ApplicationCommandOptionSubcommandOrGroup]
  | ApplicationCommandOptionsValues [ApplicationCommandOptionValue]
  deriving (Show, Eq, Read)

data ApplicationCommandOptionSubcommandOrGroup
  = ApplicationCommandOptionSubcommandGroup
      { applicationCommandOptionSubcommandGroupName :: T.Text,
        applicationCommandOptionSubcommandGroupDescription :: T.Text,
        applicationCommandOptionSubcommandGroupOptions :: [ApplicationCommandOptionSubcommand]
      }
  | ApplicationCommandOptionSubcommandOrGroupSubcommand ApplicationCommandOptionSubcommand
  deriving (Show, Eq, Read)

data ApplicationCommandOptionSubcommand = ApplicationCommandOptionSubcommand
  { applicationCommandOptionSubcommandName :: T.Text,
    applicationCommandOptionSubcommandDescription :: T.Text,
    applicationCommandOptionSubcommandOptions :: [ApplicationCommandOptionValue]
  }
  deriving (Show, Eq, Read)

data ApplicationCommandOptionValue
  = ApplicationCommandOptionValueString
      { applicationCommandOptionValueName :: T.Text,
        applicationCommandOptionValueDescription :: T.Text,
        applicationCommandOptionValueRequired :: Maybe Bool,
        applicationCommandOptionValueStringChoices :: Maybe [Choice T.Text],
        applicationCommandOptionValueAutocomplete :: Maybe Bool
      }
  | ApplicationCommandOptionValueInteger
      { applicationCommandOptionValueName :: T.Text,
        applicationCommandOptionValueDescription :: T.Text,
        applicationCommandOptionValueRequired :: Maybe Bool,
        applicationCommandOptionValueIntegerChoices :: Maybe [Choice Integer],
        applicationCommandOptionValueIntegerMinVal :: Maybe Integer,
        applicationCommandOptionValueIntegerMaxVal :: Maybe Integer,
        applicationCommandOptionValueAutocomplete :: Maybe Bool
      }
  | ApplicationCommandOptionValueBoolean
      { applicationCommandOptionValueName :: T.Text,
        applicationCommandOptionValueDescription :: T.Text,
        applicationCommandOptionValueRequired :: Maybe Bool
      }
  | ApplicationCommandOptionValueUser
      { applicationCommandOptionValueName :: T.Text,
        applicationCommandOptionValueDescription :: T.Text,
        applicationCommandOptionValueRequired :: Maybe Bool
      }
  | ApplicationCommandOptionValueChannel
      { applicationCommandOptionValueName :: T.Text,
        applicationCommandOptionValueDescription :: T.Text,
        applicationCommandOptionValueRequired :: Maybe Bool,
        applicationCommandOptionValueChannelTypes :: Maybe [ApplicationCommandChannelType]
      }
  | ApplicationCommandOptionValueRole
      { applicationCommandOptionValueName :: T.Text,
        applicationCommandOptionValueDescription :: T.Text,
        applicationCommandOptionValueRequired :: Maybe Bool
      }
  | ApplicationCommandOptionValueMentionable
      { applicationCommandOptionValueName :: T.Text,
        applicationCommandOptionValueDescription :: T.Text,
        applicationCommandOptionValueRequired :: Maybe Bool
      }
  | ApplicationCommandOptionValueNumber
      { applicationCommandOptionValueName :: T.Text,
        applicationCommandOptionValueDescription :: T.Text,
        applicationCommandOptionValueRequired :: Maybe Bool,
        applicationCommandOptionValueNumberChoices :: Maybe [Choice Scientific],
        applicationCommandOptionValueNumberMinVal :: Maybe Scientific,
        applicationCommandOptionValueNumberMaxVal :: Maybe Scientific,
        applicationCommandOptionValueAutocomplete :: Maybe Bool
      }
  deriving (Show, Eq, Read)

instance Internals ApplicationCommandOptionValue InternalApplicationCommandOption where
  toInternal ApplicationCommandOptionValueNumber {..} = InternalApplicationCommandOption ApplicationCommandOptionTypeNumber applicationCommandOptionValueName applicationCommandOptionValueDescription applicationCommandOptionValueRequired (((StringNumberValueNumber <$>) <$>) <$> applicationCommandOptionValueNumberChoices) Nothing Nothing applicationCommandOptionValueNumberMinVal applicationCommandOptionValueNumberMaxVal applicationCommandOptionValueAutocomplete
  toInternal ApplicationCommandOptionValueInteger {..} = InternalApplicationCommandOption ApplicationCommandOptionTypeInteger applicationCommandOptionValueName applicationCommandOptionValueDescription applicationCommandOptionValueRequired (((StringNumberValueInteger <$>) <$>) <$> applicationCommandOptionValueIntegerChoices) Nothing Nothing (fromInteger <$> applicationCommandOptionValueIntegerMinVal) (fromInteger <$> applicationCommandOptionValueIntegerMaxVal) applicationCommandOptionValueAutocomplete
  toInternal ApplicationCommandOptionValueString {..} = InternalApplicationCommandOption ApplicationCommandOptionTypeInteger applicationCommandOptionValueName applicationCommandOptionValueDescription applicationCommandOptionValueRequired (((StringNumberValueString <$>) <$>) <$> applicationCommandOptionValueStringChoices) Nothing Nothing Nothing Nothing applicationCommandOptionValueAutocomplete
  toInternal ApplicationCommandOptionValueChannel {..} = InternalApplicationCommandOption ApplicationCommandOptionTypeChannel applicationCommandOptionValueName applicationCommandOptionValueDescription applicationCommandOptionValueRequired Nothing Nothing applicationCommandOptionValueChannelTypes Nothing Nothing Nothing
  toInternal ApplicationCommandOptionValueBoolean {..} = InternalApplicationCommandOption ApplicationCommandOptionTypeBoolean applicationCommandOptionValueName applicationCommandOptionValueDescription applicationCommandOptionValueRequired Nothing Nothing Nothing Nothing Nothing Nothing
  toInternal ApplicationCommandOptionValueUser {..} = InternalApplicationCommandOption ApplicationCommandOptionTypeUser applicationCommandOptionValueName applicationCommandOptionValueDescription applicationCommandOptionValueRequired Nothing Nothing Nothing Nothing Nothing Nothing
  toInternal ApplicationCommandOptionValueRole {..} = InternalApplicationCommandOption ApplicationCommandOptionTypeRole applicationCommandOptionValueName applicationCommandOptionValueDescription applicationCommandOptionValueRequired Nothing Nothing Nothing Nothing Nothing Nothing
  toInternal ApplicationCommandOptionValueMentionable {..} = InternalApplicationCommandOption ApplicationCommandOptionTypeMentionable applicationCommandOptionValueName applicationCommandOptionValueDescription applicationCommandOptionValueRequired Nothing Nothing Nothing Nothing Nothing Nothing

  fromInternal InternalApplicationCommandOption {internalApplicationCommandOptionType = ApplicationCommandOptionTypeNumber, ..} = do
    cs <- maybe (Just []) (mapM extractChoices) internalApplicationCommandOptionChoices
    return $ ApplicationCommandOptionValueNumber internalApplicationCommandOptionName internalApplicationCommandOptionDescription internalApplicationCommandOptionRequired (fromResult cs) internalApplicationCommandOptionMinVal internalApplicationCommandOptionMaxVal internalApplicationCommandOptionAutocomplete
    where
      extractChoices (Choice s (StringNumberValueNumber n)) = Just (Choice s n)
      extractChoices _ = Nothing
      fromResult [] = Nothing
      fromResult is = Just is
  fromInternal InternalApplicationCommandOption {internalApplicationCommandOptionType = ApplicationCommandOptionTypeInteger, ..} = do
    cs <- maybe (Just []) (mapM extractChoices) internalApplicationCommandOptionChoices
    return $ ApplicationCommandOptionValueInteger internalApplicationCommandOptionName internalApplicationCommandOptionDescription internalApplicationCommandOptionRequired (fromResult cs) (round <$> internalApplicationCommandOptionMinVal) (round <$> internalApplicationCommandOptionMaxVal) internalApplicationCommandOptionAutocomplete
    where
      extractChoices (Choice s (StringNumberValueInteger n)) = Just (Choice s n)
      extractChoices _ = Nothing
      fromResult [] = Nothing
      fromResult is = Just is
  -- note with the above: the bounds are rounded for simplicity but ideally they wouldn't be
  fromInternal InternalApplicationCommandOption {internalApplicationCommandOptionType = ApplicationCommandOptionTypeString, ..} = do
    cs <- maybe (Just []) (mapM extractChoices) internalApplicationCommandOptionChoices
    return $ ApplicationCommandOptionValueString internalApplicationCommandOptionName internalApplicationCommandOptionDescription internalApplicationCommandOptionRequired (fromResult cs) internalApplicationCommandOptionAutocomplete
    where
      extractChoices (Choice s (StringNumberValueString n)) = Just (Choice s n)
      extractChoices _ = Nothing
      fromResult [] = Nothing
      fromResult is = Just is
  fromInternal InternalApplicationCommandOption {internalApplicationCommandOptionType = ApplicationCommandOptionTypeBoolean, ..} = Just $ ApplicationCommandOptionValueBoolean internalApplicationCommandOptionName internalApplicationCommandOptionDescription internalApplicationCommandOptionRequired
  fromInternal InternalApplicationCommandOption {internalApplicationCommandOptionType = ApplicationCommandOptionTypeUser, ..} = Just $ ApplicationCommandOptionValueUser internalApplicationCommandOptionName internalApplicationCommandOptionDescription internalApplicationCommandOptionRequired
  fromInternal InternalApplicationCommandOption {internalApplicationCommandOptionType = ApplicationCommandOptionTypeRole, ..} = Just $ ApplicationCommandOptionValueRole internalApplicationCommandOptionName internalApplicationCommandOptionDescription internalApplicationCommandOptionRequired
  fromInternal InternalApplicationCommandOption {internalApplicationCommandOptionType = ApplicationCommandOptionTypeMentionable, ..} = Just $ ApplicationCommandOptionValueMentionable internalApplicationCommandOptionName internalApplicationCommandOptionDescription internalApplicationCommandOptionRequired
  fromInternal InternalApplicationCommandOption {internalApplicationCommandOptionType = ApplicationCommandOptionTypeChannel, ..} = Just $ ApplicationCommandOptionValueChannel internalApplicationCommandOptionName internalApplicationCommandOptionDescription internalApplicationCommandOptionRequired internalApplicationCommandOptionChannelTypes
  fromInternal _ = Nothing

instance Internals ApplicationCommandOptionSubcommand InternalApplicationCommandOption where
  toInternal ApplicationCommandOptionSubcommand {..} = InternalApplicationCommandOption ApplicationCommandOptionTypeSubcommand applicationCommandOptionSubcommandName applicationCommandOptionSubcommandDescription Nothing Nothing (Just $ toInternal <$> applicationCommandOptionSubcommandOptions) Nothing Nothing Nothing Nothing

  fromInternal InternalApplicationCommandOption {internalApplicationCommandOptionType = ApplicationCommandOptionTypeSubcommand, ..} = do
    os <- maybe (Just []) (mapM fromInternal) internalApplicationCommandOptionOptions
    return $ ApplicationCommandOptionSubcommand internalApplicationCommandOptionName internalApplicationCommandOptionDescription os
  fromInternal _ = Nothing

instance Internals ApplicationCommandOptionSubcommandOrGroup InternalApplicationCommandOption where
  toInternal (ApplicationCommandOptionSubcommandOrGroupSubcommand s) = toInternal s
  toInternal ApplicationCommandOptionSubcommandGroup {..} = InternalApplicationCommandOption ApplicationCommandOptionTypeSubcommandGroup applicationCommandOptionSubcommandGroupName applicationCommandOptionSubcommandGroupDescription Nothing Nothing (Just $ toInternal <$> applicationCommandOptionSubcommandGroupOptions) Nothing Nothing Nothing Nothing

  fromInternal io@InternalApplicationCommandOption {internalApplicationCommandOptionType = ApplicationCommandOptionTypeSubcommand, ..} = ApplicationCommandOptionSubcommandOrGroupSubcommand <$> fromInternal io
  fromInternal InternalApplicationCommandOption {internalApplicationCommandOptionType = ApplicationCommandOptionTypeSubcommandGroup, ..} = do
    os <- maybe (Just []) (mapM fromInternal) internalApplicationCommandOptionOptions
    return $ ApplicationCommandOptionSubcommandGroup internalApplicationCommandOptionName internalApplicationCommandOptionDescription os
  fromInternal _ = Nothing

instance Internals ApplicationCommandOptions [InternalApplicationCommandOption] where
  toInternal (ApplicationCommandOptionsSubcommands is) = toInternal <$> is
  toInternal (ApplicationCommandOptionsValues is) = toInternal <$> is

  fromInternal is = (ApplicationCommandOptionsSubcommands <$> mapM fromInternal is) <|> (ApplicationCommandOptionsValues <$> mapM fromInternal is)

instance Internals ApplicationCommand InternalApplicationCommand where
  toInternal ApplicationCommandUser {..} = InternalApplicationCommand applicationCommandId (Just ApplicationCommandTypeUser) applicationCommandApplicationId applicationCommandGuildId applicationCommandName "" Nothing applicationCommandDefaultPermission applicationCommandVersion
  toInternal ApplicationCommandMessage {..} = InternalApplicationCommand applicationCommandId (Just ApplicationCommandTypeMessage) applicationCommandApplicationId applicationCommandGuildId applicationCommandName "" Nothing applicationCommandDefaultPermission applicationCommandVersion
  toInternal ApplicationCommandChatInput {..} = InternalApplicationCommand applicationCommandId (Just ApplicationCommandTypeChatInput) applicationCommandApplicationId applicationCommandGuildId applicationCommandName applicationCommandDescription (toInternal <$> applicationCommandOptions) applicationCommandDefaultPermission applicationCommandVersion
  toInternal (ApplicationCommandUnknown ai) = ai

  fromInternal InternalApplicationCommand {internalApplicationCommandType = Just ApplicationCommandTypeUser, ..} = Just $ ApplicationCommandUser internalApplicationCommandId internalApplicationCommandApplicationId internalApplicationCommandGuildId internalApplicationCommandName internalApplicationCommandDefaultPermission internalApplicationCommandVersion
  fromInternal InternalApplicationCommand {internalApplicationCommandType = Just ApplicationCommandTypeMessage, ..} = Just $ ApplicationCommandMessage internalApplicationCommandId internalApplicationCommandApplicationId internalApplicationCommandGuildId internalApplicationCommandName internalApplicationCommandDefaultPermission internalApplicationCommandVersion
  fromInternal a@InternalApplicationCommand {internalApplicationCommandType = Just ApplicationCommandTypeChatInput, ..} = Just $ fromMaybe (ApplicationCommandUnknown a) $ ((internalApplicationCommandOptions <|> Just []) >>= fromInternal) >>= \iOptions -> Just $ ApplicationCommandChatInput internalApplicationCommandId internalApplicationCommandApplicationId internalApplicationCommandGuildId internalApplicationCommandName internalApplicationCommandDescription (Just iOptions) internalApplicationCommandDefaultPermission internalApplicationCommandVersion
  fromInternal a = fromInternal (a {internalApplicationCommandType = Just ApplicationCommandTypeChatInput})

-- Just $ ApplicationCommandMessage internalApplicationCommandId internalApplicationCommandApplicationId internalApplicationCommandGuildId internalApplicationCommandName internalApplicationCommandDefaultPermission internalApplicationCommandVersion

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
    -- non-slash commands.
    createApplicationCommandOptions :: Maybe [InternalApplicationCommandOption],
    -- | Whether the command is enabled by default when the application is added
    -- to a guild. Defaults to true if not present
    createApplicationCommandDefaultPermission :: Maybe Bool,
    -- | What the type of the command is. If `Nothing`, defaults to slash
    -- commands.
    createApplicationCommandType :: Maybe ApplicationCommandType
  }
  deriving (Show, Eq, Read)

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

-- | Create the basics for a chat input (slash command). Use record overwriting
-- to enter the other values. The name needs to be all lower case letters, and
-- between 1 and 32 characters. The description has to be non-empty and less
-- than or equal to 100 characters.
createApplicationCommandChatInput :: T.Text -> T.Text -> Maybe CreateApplicationCommand
createApplicationCommandChatInput name desc
  | T.all isLower name && not (T.null desc) && l >= 1 && l <= 32 && T.length desc <= 100 = Just $ CreateApplicationCommand name desc Nothing Nothing (Just ApplicationCommandTypeChatInput)
  | otherwise = Nothing
  where
    l = T.length name

-- | Create the basics for a user command. Use record overwriting to enter the
-- other values. The name needs to be between 1 and 32 characters.
createApplicationCommandUser :: T.Text -> Maybe CreateApplicationCommand
createApplicationCommandUser name
  | l >= 1 && l <= 32 = Just $ CreateApplicationCommand name "" Nothing Nothing (Just ApplicationCommandTypeUser)
  | otherwise = Nothing
  where
    l = T.length name

-- | Create the basics for a message command. Use record overwriting to enter
-- the other values. The name needs to be between 1 and 32 characters.
createApplicationCommandMessage :: T.Text -> Maybe CreateApplicationCommand
createApplicationCommandMessage name
  | l >= 1 && l <= 32 = Just $ CreateApplicationCommand name "" Nothing Nothing (Just ApplicationCommandTypeMessage)
  | otherwise = Nothing
  where
    l = T.length name

-- | Data type to be used when editing application commands. The specification
-- is below. See `CreateApplicationCommand` for an explanation for the
-- parameters.
--
-- https://discord.com/developers/docs/interactions/application-commands#edit-global-application-command
data EditApplicationCommand = EditApplicationCommand
  { editApplicationCommandName :: Maybe T.Text,
    editApplicationCommandDescription :: Maybe T.Text,
    editApplicationCommandOptions :: Maybe [InternalApplicationCommandOption],
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
data InternalApplicationCommand = InternalApplicationCommand
  { -- | Unique id of the command.
    internalApplicationCommandId :: ApplicationCommandId,
    -- | The type of the command.
    internalApplicationCommandType :: Maybe ApplicationCommandType,
    -- | Unique id of the parent application (the bot).
    internalApplicationCommandApplicationId :: ApplicationId,
    -- | The guild id of the command if not global.
    internalApplicationCommandGuildId :: Maybe GuildId,
    -- | Must be 1-32 characters.
    internalApplicationCommandName :: T.Text,
    -- | Must be empty for USER and MESSAGE commands, otherwise 1-100 chars.
    internalApplicationCommandDescription :: T.Text,
    -- | CHAT_INPUT only, parameters to command
    internalApplicationCommandOptions :: Maybe [InternalApplicationCommandOption],
    -- | whether the command is enabled by default when the app is added to a
    -- guild. Defaults to true.
    internalApplicationCommandDefaultPermission :: Maybe Bool,
    internalApplicationCommandVersion :: Snowflake
  }
  deriving (Show, Eq, Read)

instance FromJSON InternalApplicationCommand where
  parseJSON =
    withObject
      "InternalApplicationCommand"
      ( \v ->
          InternalApplicationCommand
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
data InternalApplicationCommandOption = InternalApplicationCommandOption
  { -- | What the type of this option is.
    internalApplicationCommandOptionType :: ApplicationCommandOptionType,
    -- | The name of the option . 1-32 characters
    internalApplicationCommandOptionName :: T.Text,
    -- | 1-100 characters
    internalApplicationCommandOptionDescription :: T.Text,
    -- | Is the parameter required? default false
    internalApplicationCommandOptionRequired :: Maybe Bool,
    -- | If specified, these are the only valid options to choose from. Type
    -- depends on optionType, and can only be specified for STRING, INTEGER or
    -- NUMBER types.
    internalApplicationCommandOptionChoices :: Maybe [InternalApplicationCommandOptionChoice],
    -- | If the option type is a subcommand or subcommand group type, these are
    -- the parameters to the subcommand.
    internalApplicationCommandOptionOptions :: Maybe [InternalApplicationCommandOption],
    -- | If option is channel type, these are the only channel types allowed.
    internalApplicationCommandOptionChannelTypes :: Maybe [ApplicationCommandChannelType],
    -- | If option is number type, minimum value for the number
    internalApplicationCommandOptionMinVal :: Maybe Scientific,
    -- | if option is number type, maximum value for the number
    internalApplicationCommandOptionMaxVal :: Maybe Scientific,
    -- | Enable auto complete interactions. may not be set to true if choices is present.
    internalApplicationCommandOptionAutocomplete :: Maybe Bool
  }
  deriving (Show, Eq, Read)

instance ToJSON InternalApplicationCommandOption where
  toJSON InternalApplicationCommandOption {..} =
    object
      [ (name, value)
        | (name, Just value) <-
            [ ("type", toMaybeJSON internalApplicationCommandOptionType),
              ("name", toMaybeJSON internalApplicationCommandOptionName),
              ("description", toMaybeJSON internalApplicationCommandOptionDescription),
              ("required", toJSON <$> internalApplicationCommandOptionRequired),
              ("choices", toJSON <$> internalApplicationCommandOptionChoices),
              ("options", toJSON <$> internalApplicationCommandOptionOptions),
              ("channel_types", toJSON <$> internalApplicationCommandOptionChannelTypes),
              ("min_val", toJSON <$> internalApplicationCommandOptionMinVal),
              ("max_val", toJSON <$> internalApplicationCommandOptionMaxVal),
              ("autocomplete", toJSON <$> internalApplicationCommandOptionAutocomplete)
            ]
      ]

instance FromJSON InternalApplicationCommandOption where
  parseJSON =
    withObject
      "InternalApplicationCommandOption"
      ( \v ->
          InternalApplicationCommandOption
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
    ApplicationCommandOptionTypeSubcommand
  | -- | A subcommand group. It can take further options, excluding sub command
    -- groups.
    ApplicationCommandOptionTypeSubcommandGroup
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
  fromEnum ApplicationCommandOptionTypeSubcommand = 1
  fromEnum ApplicationCommandOptionTypeSubcommandGroup = 2
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
      table = makeTable ApplicationCommandOptionTypeSubcommand

instance ToJSON ApplicationCommandOptionType where
  toJSON = toJSON . fromEnum

instance FromJSON ApplicationCommandOptionType where
  parseJSON = withScientific "ApplicationCommandOptionType" (return . toEnum . round)

-- | Utility data type to store strings or number types.
data StringNumberValue = StringNumberValueString T.Text | StringNumberValueNumber Scientific | StringNumberValueInteger Integer
  deriving (Show, Read, Eq)

instance ToJSON StringNumberValue where
  toJSON (StringNumberValueString s) = toJSON s
  toJSON (StringNumberValueNumber i) = toJSON i
  toJSON (StringNumberValueInteger i) = toJSON i

instance FromJSON StringNumberValue where
  parseJSON (String t) = return $ StringNumberValueString t
  parseJSON v = (StringNumberValueInteger <$> parseJSON v) <|> (StringNumberValueNumber <$> parseJSON v)

data Choice a = Choice {choiceName :: T.Text, choiceValue :: a}
  deriving (Show, Read, Eq)

instance Functor Choice where
  fmap f (Choice s a) = Choice s (f a)

type InternalApplicationCommandOptionChoice = Choice StringNumberValue

-- | The choices for a particular option.
-- data InternalApplicationCommandOptionChoice = InternalApplicationCommandOptionChoice
--   { internalApplicationCommandOptionChoiceName :: T.Text,
--     internalApplicationCommandOptionChoiceValue :: StringNumberValue
--   }
--   deriving (Show, Read, Eq)
instance (ToJSON a) => ToJSON (Choice a) where
  toJSON Choice {..} = object [("name", toJSON choiceName), ("value", toJSON choiceValue)]

instance (FromJSON a) => FromJSON (Choice a) where
  parseJSON =
    withObject
      "Choice"
      ( \v ->
          Choice
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
