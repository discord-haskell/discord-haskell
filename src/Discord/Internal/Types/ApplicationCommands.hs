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
    ApplicationCommandOptions (..),
    ApplicationCommandOptionSubcommandOrGroup (..),
    ApplicationCommandOptionSubcommand (..),
    ApplicationCommandOptionValue (..),
    createApplicationCommandChatInput,
    createApplicationCommandUser,
    createApplicationCommandMessage,
    CreateApplicationCommand
      ( createApplicationCommandName,
        createApplicationCommandDescription,
        createApplicationCommandOptions,
        createApplicationCommandDefaultPermission
      ),
    EditApplicationCommand (..),
    defaultEditApplicationCommand,
    Choice (..),
    ApplicationCommandChannelType (..),
    GuildApplicationCommandPermissions (..),
    ApplicationCommandPermissions (..),
  )
where

import Data.Aeson
import Data.Aeson.Types (Pair, Parser)
import Data.Data (Data)
import Data.Foldable (Foldable (toList))
import Data.Scientific (Scientific)
import qualified Data.Text as T
import Discord.Internal.Types.Prelude (ApplicationCommandId, ApplicationId, GuildId, InternalDiscordType (..), Snowflake, discordTypeParseJSON, toMaybeJSON)

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
        -- | The description of the application command.
        applicationCommandDescription :: T.Text,
        -- | The parameters for the command.
        applicationCommandOptions :: Maybe ApplicationCommandOptions,
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
          defPerm <- v .:? "default_permission" .!= True
          version <- v .: "version"
          t <- v .:? "type" :: Parser (Maybe Int)
          case t of
            (Just 2) -> return $ ApplicationCommandUser acid aid gid name defPerm version
            (Just 3) -> return $ ApplicationCommandMessage acid aid gid name defPerm version
            _ -> do
              desc <- v .: "description"
              options <- v .:? "options"
              return $ ApplicationCommandChatInput acid aid gid name desc options defPerm version
      )

-- | Either subcommands and groups, or values.
data ApplicationCommandOptions
  = ApplicationCommandOptionsSubcommands [ApplicationCommandOptionSubcommandOrGroup]
  | ApplicationCommandOptionsValues [ApplicationCommandOptionValue]
  deriving (Show, Eq, Read)

instance FromJSON ApplicationCommandOptions where
  parseJSON =
    withArray
      "ApplicationCommandOptions"
      ( \a -> do
          let a' = toList a
          case a' of
            [] -> return $ ApplicationCommandOptionsValues []
            (v' : _) ->
              withObject
                "ApplicationCommandOptions item"
                ( \v -> do
                    t <- v .: "type" :: Parser Int
                    if t == 1 || t == 2
                      then ApplicationCommandOptionsSubcommands <$> mapM parseJSON a'
                      else ApplicationCommandOptionsValues <$> mapM parseJSON a'
                )
                v'
      )

instance ToJSON ApplicationCommandOptions where
  toJSON (ApplicationCommandOptionsSubcommands o) = toJSON o
  toJSON (ApplicationCommandOptionsValues o) = toJSON o

-- | Either a subcommand group or a subcommand.
data ApplicationCommandOptionSubcommandOrGroup
  = ApplicationCommandOptionSubcommandGroup
      { -- | The name of the subcommand group
        applicationCommandOptionSubcommandGroupName :: T.Text,
        -- | The description of the subcommand group
        applicationCommandOptionSubcommandGroupDescription :: T.Text,
        -- | The subcommands in this subcommand group
        applicationCommandOptionSubcommandGroupOptions :: [ApplicationCommandOptionSubcommand]
      }
  | ApplicationCommandOptionSubcommandOrGroupSubcommand ApplicationCommandOptionSubcommand
  deriving (Show, Eq, Read)

instance FromJSON ApplicationCommandOptionSubcommandOrGroup where
  parseJSON =
    withObject
      "ApplicationCommandOptionSubcommandOrGroup"
      ( \v -> do
          t <- v .: "type" :: Parser Int
          case t of
            2 ->
              ApplicationCommandOptionSubcommandGroup
                <$> v .: "name"
                <*> v .: "description"
                <*> v .: "options"
            1 -> ApplicationCommandOptionSubcommandOrGroupSubcommand <$> parseJSON (Object v)
            _ -> fail "unexpected subcommand group type"
      )

instance ToJSON ApplicationCommandOptionSubcommandOrGroup where
  toJSON ApplicationCommandOptionSubcommandGroup {..} =
    object
      [ ("type", Number 2),
        ("name", toJSON applicationCommandOptionSubcommandGroupName),
        ("description", toJSON applicationCommandOptionSubcommandGroupDescription),
        ("options", toJSON applicationCommandOptionSubcommandGroupOptions)
      ]
  toJSON (ApplicationCommandOptionSubcommandOrGroupSubcommand a) = toJSON a

-- | Data for a single subcommand.
data ApplicationCommandOptionSubcommand = ApplicationCommandOptionSubcommand
  { -- | The name of the subcommand
    applicationCommandOptionSubcommandName :: T.Text,
    -- | The description of the subcommand
    applicationCommandOptionSubcommandDescription :: T.Text,
    -- | What options are there in this subcommand
    applicationCommandOptionSubcommandOptions :: [ApplicationCommandOptionValue]
  }
  deriving (Show, Eq, Read)

instance FromJSON ApplicationCommandOptionSubcommand where
  parseJSON =
    withObject
      "ApplicationCommandOptionSubcommand"
      ( \v -> do
          t <- v .: "type" :: Parser Int
          case t of
            1 ->
              ApplicationCommandOptionSubcommand
                <$> v .: "name"
                <*> v .: "description"
                <*> v .: "options"
            _ -> fail "unexpected subcommand type"
      )

instance ToJSON ApplicationCommandOptionSubcommand where
  toJSON ApplicationCommandOptionSubcommand {..} =
    object
      [ ("type", Number 1),
        ("name", toJSON applicationCommandOptionSubcommandName),
        ("description", toJSON applicationCommandOptionSubcommandDescription),
        ("options", toJSON applicationCommandOptionSubcommandOptions)
      ]

-- | Data for a single value.
data ApplicationCommandOptionValue
  = ApplicationCommandOptionValueString
      { -- | The name of the value
        applicationCommandOptionValueName :: T.Text,
        -- | The description of the value
        applicationCommandOptionValueDescription :: T.Text,
        -- | Whether this option is required
        applicationCommandOptionValueRequired :: Bool,
        -- | Whether to autocomplete or have a list of named choices. For neither option, use `Left False`
        applicationCommandOptionValueStringChoices :: AutocompleteOrChoice T.Text
      }
  | ApplicationCommandOptionValueInteger
      { -- | The name of the value
        applicationCommandOptionValueName :: T.Text,
        -- | The description of the value
        applicationCommandOptionValueDescription :: T.Text,
        -- | Whether this option is required
        applicationCommandOptionValueRequired :: Bool,
        -- | Whether to autocomplete or have a list of named choices. For neither option, use `Left False`
        applicationCommandOptionValueIntegerChoices :: AutocompleteOrChoice Integer,
        -- | The lower bound of values permitted. If choices are provided or autocomplete is on, this can be ignored
        applicationCommandOptionValueIntegerMinVal :: Maybe Integer,
        -- | The upper bound of values permitted. If choices are provided or autocomplete is on, this can be ignored
        applicationCommandOptionValueIntegerMaxVal :: Maybe Integer
      }
  | ApplicationCommandOptionValueBoolean
      { -- | The name of the value
        applicationCommandOptionValueName :: T.Text,
        -- | The description of the value
        applicationCommandOptionValueDescription :: T.Text,
        -- | Whether this option is required
        applicationCommandOptionValueRequired :: Bool
      }
  | ApplicationCommandOptionValueUser
      { -- | The name of the value
        applicationCommandOptionValueName :: T.Text,
        -- | The description of the value
        applicationCommandOptionValueDescription :: T.Text,
        -- | Whether this option is required
        applicationCommandOptionValueRequired :: Bool
      }
  | ApplicationCommandOptionValueChannel
      { -- | The name of the value
        applicationCommandOptionValueName :: T.Text,
        -- | The description of the value
        applicationCommandOptionValueDescription :: T.Text,
        -- | Whether this option is required
        applicationCommandOptionValueRequired :: Bool,
        -- | What type of channel can be put in here
        applicationCommandOptionValueChannelTypes :: Maybe [ApplicationCommandChannelType]
      }
  | ApplicationCommandOptionValueRole
      { -- | The name of the value
        applicationCommandOptionValueName :: T.Text,
        -- | The description of the value
        applicationCommandOptionValueDescription :: T.Text,
        -- | Whether this option is required
        applicationCommandOptionValueRequired :: Bool
      }
  | ApplicationCommandOptionValueMentionable
      { -- | The name of the value
        applicationCommandOptionValueName :: T.Text,
        -- | The description of the value
        applicationCommandOptionValueDescription :: T.Text,
        -- | Whether this option is required
        applicationCommandOptionValueRequired :: Bool
      }
  | ApplicationCommandOptionValueNumber
      { -- | The name of the value
        applicationCommandOptionValueName :: T.Text,
        -- | The description of the value
        applicationCommandOptionValueDescription :: T.Text,
        -- | Whether this option is required
        applicationCommandOptionValueRequired :: Bool,
        -- | Whether to autocomplete or have a list of named choices. For neither option, use `Left False`
        applicationCommandOptionValueNumberChoices :: AutocompleteOrChoice Scientific,
        -- | The lower bound of values permitted. If choices are provided or autocomplete is on, this can be ignored
        applicationCommandOptionValueNumberMinVal :: Maybe Scientific,
        -- | The upper bound of values permitted. If choices are provided or autocomplete is on, this can be ignored
        applicationCommandOptionValueNumberMaxVal :: Maybe Scientific
      }
  deriving (Show, Eq, Read)

instance FromJSON ApplicationCommandOptionValue where
  parseJSON =
    withObject
      "ApplicationCommandOptionValue"
      ( \v -> do
          name <- v .: "name"
          desc <- v .: "description"
          required <- v .:? "required" .!= False
          t <- v .: "type" :: Parser Int
          case t of
            3 ->
              ApplicationCommandOptionValueString name desc required
                <$> parseJSON (Object v)
            4 ->
              ApplicationCommandOptionValueInteger name desc required
                <$> parseJSON (Object v)
                <*> v .:? "min_value"
                <*> v .:? "max_value"
            10 ->
              ApplicationCommandOptionValueNumber name desc required
                <$> parseJSON (Object v)
                <*> v .:? "min_value"
                <*> v .:? "max_value"
            7 ->
              ApplicationCommandOptionValueChannel name desc required
                <$> v .:? "channel_types"
            5 -> return $ ApplicationCommandOptionValueBoolean name desc required
            6 -> return $ ApplicationCommandOptionValueUser name desc required
            8 -> return $ ApplicationCommandOptionValueRole name desc required
            9 -> return $ ApplicationCommandOptionValueMentionable name desc required
            _ -> fail "unknown application command option value type"
      )

instance ToJSON ApplicationCommandOptionValue where
  toJSON ApplicationCommandOptionValueString {..} =
    object
      [ ("type", Number 3),
        ("name", toJSON applicationCommandOptionValueName),
        ("description", toJSON applicationCommandOptionValueDescription),
        ("required", toJSON applicationCommandOptionValueRequired),
        choiceOrAutocompleteToJSON applicationCommandOptionValueStringChoices
      ]
  toJSON ApplicationCommandOptionValueInteger {..} =
    object
      [ ("type", Number 4),
        ("name", toJSON applicationCommandOptionValueName),
        ("description", toJSON applicationCommandOptionValueDescription),
        ("required", toJSON applicationCommandOptionValueRequired),
        choiceOrAutocompleteToJSON applicationCommandOptionValueIntegerChoices
      ]
  toJSON ApplicationCommandOptionValueNumber {..} =
    object
      [ ("type", Number 10),
        ("name", toJSON applicationCommandOptionValueName),
        ("description", toJSON applicationCommandOptionValueDescription),
        ("required", toJSON applicationCommandOptionValueRequired),
        choiceOrAutocompleteToJSON applicationCommandOptionValueNumberChoices
      ]
  toJSON ApplicationCommandOptionValueChannel {..} =
    object
      [ ("type", Number 7),
        ("name", toJSON applicationCommandOptionValueName),
        ("description", toJSON applicationCommandOptionValueDescription),
        ("required", toJSON applicationCommandOptionValueRequired),
        ("channel_types", toJSON applicationCommandOptionValueChannelTypes)
      ]
  toJSON acov =
    object
      [ ("type", Number (t acov)),
        ("name", toJSON $ applicationCommandOptionValueName acov),
        ("description", toJSON $ applicationCommandOptionValueDescription acov),
        ("required", toJSON $ applicationCommandOptionValueRequired acov)
      ]
    where
      t ApplicationCommandOptionValueBoolean {} = 5
      t ApplicationCommandOptionValueUser {} = 6
      t ApplicationCommandOptionValueRole {} = 8
      t ApplicationCommandOptionValueMentionable {} = 9
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
        createApplicationCommandName :: T.Text,
        -- | The application command description (1-100 chars). Has to be empty for
        -- non-slash commands.
        createApplicationCommandDescription :: T.Text,
        -- | What options the application (max length 25). Has to be `Nothing` for
        -- non-slash commands.
        createApplicationCommandOptions :: Maybe ApplicationCommandOptions,
        -- | Whether the command is enabled by default when the application is added
        -- to a guild.
        createApplicationCommandDefaultPermission :: Bool
      }
  | CreateApplicationCommandUser
      { -- | The application command name (1-32 chars).
        createApplicationCommandName :: T.Text,
        -- | Whether the command is enabled by default when the application is added
        -- to a guild.
        createApplicationCommandDefaultPermission :: Bool
      }
  | CreateApplicationCommandMessage
      { -- | The application command name (1-32 chars).
        createApplicationCommandName :: T.Text,
        -- | Whether the command is enabled by default when the application is added
        -- to a guild.
        createApplicationCommandDefaultPermission :: Bool
      }
  deriving (Show, Eq, Read)

instance ToJSON CreateApplicationCommand where
  toJSON CreateApplicationCommandChatInput {..} =
    object
      [ (name, value)
        | (name, Just value) <-
            [ ("name", toMaybeJSON createApplicationCommandName),
              ("description", toMaybeJSON createApplicationCommandDescription),
              ("options", toJSON <$> createApplicationCommandOptions),
              ("default_permission", toMaybeJSON createApplicationCommandDefaultPermission),
              ("type", Just $ Number 1)
            ]
      ]
  toJSON CreateApplicationCommandUser {..} =
    object
      [ (name, value)
        | (name, Just value) <-
            [ ("name", toMaybeJSON createApplicationCommandName),
              ("default_permission", toMaybeJSON createApplicationCommandDefaultPermission),
              ("type", Just $ Number 2)
            ]
      ]
  toJSON CreateApplicationCommandMessage {..} =
    object
      [ (name, value)
        | (name, Just value) <-
            [ ("name", toMaybeJSON createApplicationCommandName),
              ("default_permission", toMaybeJSON createApplicationCommandDefaultPermission),
              ("type", Just $ Number 3)
            ]
      ]

nameIsValid :: Bool -> T.Text -> Bool
nameIsValid isChatInput name = l >= 1 && l <= 32 && (isChatInput <= T.all (`elem` validChars) name)
  where
    l = T.length name
    validChars = '-' : ['a' .. 'z']

-- | Create the basics for a chat input (slash command). Use record overwriting
-- to enter the other values. The name needs to be all lower case letters, and
-- between 1 and 32 characters. The description has to be non-empty and less
-- than or equal to 100 characters.
createApplicationCommandChatInput :: T.Text -> T.Text -> Maybe CreateApplicationCommand
createApplicationCommandChatInput name desc
  | nameIsValid True name && not (T.null desc) && T.length desc <= 100 = Just $ CreateApplicationCommandChatInput name desc Nothing True
  | otherwise = Nothing

-- | Create the basics for a user command. Use record overwriting to enter the
-- other values. The name needs to be between 1 and 32 characters.
createApplicationCommandUser :: T.Text -> Maybe CreateApplicationCommand
createApplicationCommandUser name
  | nameIsValid False name = Just $ CreateApplicationCommandUser name True
  | otherwise = Nothing

-- | Create the basics for a message command. Use record overwriting to enter
-- the other values. The name needs to be between 1 and 32 characters.
createApplicationCommandMessage :: T.Text -> Maybe CreateApplicationCommand
createApplicationCommandMessage name
  | nameIsValid False name = Just $ CreateApplicationCommandMessage name True
  | otherwise = Nothing

-- | Data type to be used when editing application commands. The specification
-- is below. See `CreateApplicationCommand` for an explanation for the
-- parameters.
--
-- https://discord.com/developers/docs/interactions/application-commands#edit-global-application-command
data EditApplicationCommand
  = EditApplicationCommandChatInput
      { editApplicationCommandName :: Maybe T.Text,
        editApplicationCommandDescription :: Maybe T.Text,
        editApplicationCommandOptions :: Maybe ApplicationCommandOptions,
        editApplicationCommandDefaultPermission :: Maybe Bool
      }
  | EditApplicationCommandUser
      { editApplicationCommandName :: Maybe T.Text,
        editApplicationCommandDefaultPermission :: Maybe Bool
      }
  | EditApplicationCommandMessage
      { editApplicationCommandName :: Maybe T.Text,
        editApplicationCommandDefaultPermission :: Maybe Bool
      }

defaultEditApplicationCommand :: Int -> EditApplicationCommand
defaultEditApplicationCommand 2 = EditApplicationCommandUser Nothing Nothing
defaultEditApplicationCommand 3 = EditApplicationCommandMessage Nothing Nothing
defaultEditApplicationCommand _ = EditApplicationCommandChatInput Nothing Nothing Nothing Nothing

instance ToJSON EditApplicationCommand where
  toJSON EditApplicationCommandChatInput {..} =
    object
      [ (name, value)
        | (name, Just value) <-
            [ ("name", toJSON <$> editApplicationCommandName),
              ("description", toJSON <$> editApplicationCommandDescription),
              ("options", toJSON <$> editApplicationCommandOptions),
              ("default_permission", toJSON <$> editApplicationCommandDefaultPermission),
              ("type", Just $ Number 1)
            ]
      ]
  toJSON EditApplicationCommandUser {..} =
    object
      [ (name, value)
        | (name, Just value) <-
            [ ("name", toJSON <$> editApplicationCommandName),
              ("default_permission", toJSON <$> editApplicationCommandDefaultPermission),
              ("type", Just $ Number 2)
            ]
      ]
  toJSON EditApplicationCommandMessage {..} =
    object
      [ (name, value)
        | (name, Just value) <-
            [ ("name", toJSON <$> editApplicationCommandName),
              ("default_permission", toJSON <$> editApplicationCommandDefaultPermission),
              ("type", Just $ Number 3)
            ]
      ]

data Choice a = Choice {choiceName :: T.Text, choiceValue :: a}
  deriving (Show, Read, Eq)

instance Functor Choice where
  fmap f (Choice s a) = Choice s (f a)

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

instance InternalDiscordType ApplicationCommandChannelType where
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
