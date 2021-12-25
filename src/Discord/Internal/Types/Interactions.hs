{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Discord.Internal.Types.Interactions
  ( Interaction (..),
    InteractionDataComponent (..),
    InteractionDataApplicationCommand (..),
    InteractionDataApplicationCommandOptions (..),
    InteractionDataApplicationCommandOptionSubcommandOrGroup (..),
    InteractionDataApplicationCommandOptionSubcommand (..),
    InteractionDataApplicationCommandOptionValue (..),
    ApplicationCommandInteractionDataValue (..),
    InternalInteraction (..),
    InteractionToken,
    InteractionType,
    InternalInteractionData (..),
    ResolvedData (..),
    InternalInteractionDataApplicationCommandOption (..),
    InteractionResponse (..),
    interactionResponseBasic,
    InteractionCallbackType (..),
    InteractionCallbackData (..),
    InteractionCallbackAutocomplete,
    InteractionCallbackMessages (..),
    interactionCallbackMessagesBasic,
    InteractionCallbackDataFlags (..),
    InteractionCallbackDataFlag (..),
  )
where

import Control.Applicative (Alternative ((<|>)))
import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Bits (Bits (shift, (.|.)))
import Data.Data (Data)
import Data.Maybe (fromJust, fromMaybe)
import Data.Scientific (Scientific)
import qualified Data.Text as T
import Discord.Internal.Types.ApplicationCommands
  (
    InternalApplicationCommandOptionChoice,
    ApplicationCommandOptionType (..),
    ApplicationCommandType (..),
  )
import Discord.Internal.Types.Channel (AllowedMentions, Attachment, Message)
import Discord.Internal.Types.Components (Component, ComponentType (..))
import Discord.Internal.Types.Embed (Embed)
import Discord.Internal.Types.Prelude (ApplicationId, ApplicationCommandId, ChannelId, GuildId, InteractionId, InteractionToken, InteractionType (..), Internals (..), MessageId, Snowflake, UserId, makeTable, toMaybeJSON)
import Discord.Internal.Types.User (GuildMember, User)

import Debug.Trace

data Interaction
  = InteractionComponent
      { interactionId :: InteractionId,
        interactionApplicationId :: ApplicationId,
        interactionDataComponent :: Maybe InteractionDataComponent, -- referenced as Data in API
        interactionGuildId :: Maybe GuildId,
        interactionChannelId :: Maybe ChannelId,
        interactionMember :: Maybe GuildMember,
        interactionUser :: Maybe User,
        interactionToken :: InteractionToken,
        interactionVersion :: Int,
        interactionMessage :: Message
      }
  | InteractionPing
      { interactionId :: InteractionId,
        interactionApplicationId :: ApplicationId,
        interactionToken :: InteractionToken,
        interactionVersion :: Int
      }
  | InteractionApplicationCommand
      { interactionId :: InteractionId,
        interactionApplicationId :: ApplicationId,
        interactionDataApplicationCommand :: Maybe InteractionDataApplicationCommand, -- referenced as Data in API
        interactionGuildId :: Maybe GuildId,
        interactionChannelId :: Maybe ChannelId,
        interactionMember :: Maybe GuildMember,
        interactionUser :: Maybe User,
        interactionToken :: InteractionToken,
        interactionVersion :: Int
      }
  | InteractionApplicationCommandAutocomplete
      { interactionId :: InteractionId,
        interactionApplicationId :: ApplicationId,
        interactionDataApplicationCommand :: Maybe InteractionDataApplicationCommand, -- referenced as Data in API
        interactionGuildId :: Maybe GuildId,
        interactionChannelId :: Maybe ChannelId,
        interactionMember :: Maybe GuildMember,
        interactionUser :: Maybe User,
        interactionToken :: InteractionToken,
        interactionVersion :: Int
      }
  | InteractionUnknown InternalInteraction
  deriving (Show, Read, Eq)

data InteractionDataComponent
  = InteractionDataComponentButton
      { -- | Component only, the unique id
        interactionDataComponentCustomId :: T.Text
      }
  | InteractionDataComponentSelectMenu
      { interactionDataComponentCustomId :: T.Text,
        interactionDataComponentValues :: [T.Text]
      }
  deriving (Show, Read, Eq)

data InteractionDataApplicationCommand
  = InteractionDataApplicationCommandUser
      { -- | id of the invoked command
        interactionDataApplicationCommandId :: ApplicationCommandId,
        -- | name of the invoked command
        interactionDataApplicationCommandName :: T.Text,
        -- | the resolved data in the command
        interactionDataApplicationCommandResolvedData :: Maybe ResolvedData,
        -- | the target of the command
        interactionDataApplicationCommandTargetId :: UserId
      }
  | InteractionDataApplicationCommandMessage
      { -- | Application command only, id of the invoked command
        interactionDataApplicationCommandId :: ApplicationCommandId,
        -- | Application command only, name of the invoked command
        interactionDataApplicationCommandName :: T.Text,
        interactionDataApplicationCommandResolvedData :: Maybe ResolvedData,
        interactionDataApplicationCommandTargetId :: MessageId
      }
  | InteractionDataApplicationCommandChatInput
      { -- | Application command only, id of the invoked command
        interactionDataApplicationCommandId :: ApplicationCommandId,
        -- | Application command only, name of the invoked command
        interactionDataApplicationCommandName :: T.Text,
        interactionDataApplicationCommandResolvedData :: Maybe ResolvedData,
        interactionDataApplicationCommandOptions :: Maybe InteractionDataApplicationCommandOptions
      }
  deriving (Show, Read, Eq)

data InteractionDataApplicationCommandOptions
  = InteractionDataApplicationCommandOptionsSubcommands [InteractionDataApplicationCommandOptionSubcommandOrGroup]
  | InteractionDataApplicationCommandOptionsValues [InteractionDataApplicationCommandOptionValue]
  deriving (Show, Read, Eq)

data InteractionDataApplicationCommandOptionSubcommandOrGroup
  = InteractionDataApplicationCommandOptionSubcommandGroup
      { interactionDataApplicationCommandOptionSubcommandGroupName :: T.Text,
        interactionDataApplicationCommandOptionSubcommandGroupOptions :: [InteractionDataApplicationCommandOptionSubcommand],
        interactionDataApplicationCommandOptionSubcommandGroupFocused :: Maybe Bool
      }
  | InteractionDataApplicationCommandOptionSubcommandOrGroupSubcommand InteractionDataApplicationCommandOptionSubcommand
  deriving (Show, Read, Eq)

data InteractionDataApplicationCommandOptionSubcommand = InteractionDataApplicationCommandOptionSubcommand
  { interactionDataApplicationCommandOptionSubcommandName :: T.Text,
    interactionDataApplicationCommandOptionSubcommandOptions :: [InteractionDataApplicationCommandOptionValue],
    interactionDataApplicationCommandOptionSubcommandFocused :: Maybe Bool
  }
  deriving (Show, Read, Eq)

data InteractionDataApplicationCommandOptionValue = InteractionDataApplicationCommandOptionValue
  { interactionDataApplicationCommandOptionValueName :: T.Text,
    interactionDataApplicationCommandOptionValueValue :: ApplicationCommandInteractionDataValue,
    interactionDataApplicationCommandOptionValueFocused :: Maybe Bool
  }
  deriving (Show, Read, Eq)

instance Internals InteractionDataApplicationCommandOptionValue InternalInteractionDataApplicationCommandOption where
  toInternal InteractionDataApplicationCommandOptionValue {..} = InternalInteractionDataApplicationCommandOption interactionDataApplicationCommandOptionValueName (getTypeFromACIDV interactionDataApplicationCommandOptionValueValue) (Just interactionDataApplicationCommandOptionValueValue) Nothing interactionDataApplicationCommandOptionValueFocused

  fromInternal InternalInteractionDataApplicationCommandOption {..}
    | internalInteractionDataApplicationCommandOptionType `elem` [ApplicationCommandOptionTypeSubcommand, ApplicationCommandOptionTypeSubcommandGroup] = Nothing
    | otherwise = do
      v <- trace ("this" ++ show internalInteractionDataApplicationCommandOptionValue) internalInteractionDataApplicationCommandOptionValue
      return $ InteractionDataApplicationCommandOptionValue internalInteractionDataApplicationCommandOptionName v internalInteractionDataApplicationCommandOptionFocused

instance Internals InteractionDataApplicationCommandOptionSubcommand InternalInteractionDataApplicationCommandOption where
  toInternal InteractionDataApplicationCommandOptionSubcommand {..} =
    InternalInteractionDataApplicationCommandOption interactionDataApplicationCommandOptionSubcommandName ApplicationCommandOptionTypeSubcommand Nothing (Just $ toInternal <$> interactionDataApplicationCommandOptionSubcommandOptions) interactionDataApplicationCommandOptionSubcommandFocused

  fromInternal InternalInteractionDataApplicationCommandOption {internalInteractionDataApplicationCommandOptionType = ApplicationCommandOptionTypeSubcommand, ..} = do
    o <- internalInteractionDataApplicationCommandOptionOptions
    o' <- mapM fromInternal o
    return $ InteractionDataApplicationCommandOptionSubcommand internalInteractionDataApplicationCommandOptionName o' internalInteractionDataApplicationCommandOptionFocused
  fromInternal _ = Nothing

instance Internals InteractionDataApplicationCommandOptionSubcommandOrGroup InternalInteractionDataApplicationCommandOption where
  toInternal InteractionDataApplicationCommandOptionSubcommandGroup {..} =
    InternalInteractionDataApplicationCommandOption interactionDataApplicationCommandOptionSubcommandGroupName ApplicationCommandOptionTypeSubcommand Nothing (Just $ toInternal <$> interactionDataApplicationCommandOptionSubcommandGroupOptions) interactionDataApplicationCommandOptionSubcommandGroupFocused
  toInternal (InteractionDataApplicationCommandOptionSubcommandOrGroupSubcommand s) = toInternal s

  fromInternal InternalInteractionDataApplicationCommandOption {internalInteractionDataApplicationCommandOptionType = ApplicationCommandOptionTypeSubcommandGroup, ..} = do
    o <- internalInteractionDataApplicationCommandOptionOptions
    o' <- mapM fromInternal o
    return $ InteractionDataApplicationCommandOptionSubcommandGroup internalInteractionDataApplicationCommandOptionName o' internalInteractionDataApplicationCommandOptionFocused
  fromInternal i@InternalInteractionDataApplicationCommandOption {internalInteractionDataApplicationCommandOptionType = ApplicationCommandOptionTypeSubcommand, ..} = InteractionDataApplicationCommandOptionSubcommandOrGroupSubcommand <$> fromInternal i
  fromInternal _ = Nothing

instance Internals InteractionDataApplicationCommandOptions [InternalInteractionDataApplicationCommandOption] where
  toInternal (InteractionDataApplicationCommandOptionsSubcommands lst) = toInternal <$> lst
  toInternal (InteractionDataApplicationCommandOptionsValues lst) = toInternal <$> lst

  fromInternal is = (InteractionDataApplicationCommandOptionsSubcommands <$> mapM fromInternal is) <|> (InteractionDataApplicationCommandOptionsValues <$> mapM fromInternal is)

instance Internals InteractionDataApplicationCommand InternalInteractionData where
  toInternal InteractionDataApplicationCommandUser {..} = InternalInteractionData (Just interactionDataApplicationCommandId) (Just interactionDataApplicationCommandName) (Just ApplicationCommandTypeUser) interactionDataApplicationCommandResolvedData Nothing Nothing Nothing Nothing Nothing
  toInternal InteractionDataApplicationCommandMessage {..} = InternalInteractionData (Just interactionDataApplicationCommandId) (Just interactionDataApplicationCommandName) (Just ApplicationCommandTypeMessage) interactionDataApplicationCommandResolvedData Nothing Nothing Nothing Nothing Nothing
  toInternal InteractionDataApplicationCommandChatInput {..} = InternalInteractionData (Just interactionDataApplicationCommandId) (Just interactionDataApplicationCommandName) (Just ApplicationCommandTypeMessage) interactionDataApplicationCommandResolvedData (toInternal <$> interactionDataApplicationCommandOptions) Nothing Nothing Nothing Nothing

  fromInternal InternalInteractionData {internalInteractionDataApplicationCommandType = Just ApplicationCommandTypeUser, ..} = do
    aid <- internalInteractionDataApplicationCommandId
    name <- internalInteractionDataApplicationCommandName
    tid <- internalInteractionDataTargetId
    return $ InteractionDataApplicationCommandUser aid name internalInteractionDataResolved tid
  fromInternal InternalInteractionData {internalInteractionDataApplicationCommandType = Just ApplicationCommandTypeMessage, ..} = do
    aid <- internalInteractionDataApplicationCommandId
    name <- internalInteractionDataApplicationCommandName
    tid <- internalInteractionDataTargetId
    return $ InteractionDataApplicationCommandMessage aid name internalInteractionDataResolved tid
  fromInternal InternalInteractionData {internalInteractionDataApplicationCommandType = Just ApplicationCommandTypeChatInput, ..} = do
    aid <- internalInteractionDataApplicationCommandId
    name <- internalInteractionDataApplicationCommandName
    return $ InteractionDataApplicationCommandChatInput aid name internalInteractionDataResolved (internalInteractionDataOptions >>= fromInternal)
  fromInternal _ = Nothing

instance Internals InteractionDataComponent InternalInteractionData where
  toInternal InteractionDataComponentButton {..} = InternalInteractionData Nothing Nothing Nothing Nothing Nothing (Just interactionDataComponentCustomId) (Just ComponentTypeButton) Nothing Nothing
  toInternal InteractionDataComponentSelectMenu {..} = InternalInteractionData Nothing Nothing Nothing Nothing Nothing (Just interactionDataComponentCustomId) (Just ComponentTypeSelectMenu) (Just interactionDataComponentValues) Nothing

  fromInternal InternalInteractionData {internalInteractionDataComponentType = Just ComponentTypeButton, ..} = InteractionDataComponentButton <$> internalInteractionDataCustomId
  fromInternal InternalInteractionData {internalInteractionDataComponentType = Just ComponentTypeSelectMenu, ..} = InteractionDataComponentSelectMenu <$> internalInteractionDataCustomId <*> internalInteractionDataValues
  fromInternal _ = Nothing

instance Internals Interaction InternalInteraction where
  toInternal InteractionPing {..} = InternalInteraction interactionId interactionApplicationId InteractionTypePing Nothing Nothing Nothing Nothing Nothing interactionToken interactionVersion Nothing
  toInternal InteractionComponent {..} = InternalInteraction interactionId interactionApplicationId InteractionTypeMessageComponent (toInternal <$> interactionDataComponent) interactionGuildId interactionChannelId interactionMember interactionUser interactionToken interactionVersion (Just interactionMessage)
  toInternal InteractionApplicationCommand {..} = InternalInteraction interactionId interactionApplicationId InteractionTypeApplicationCommand (toInternal <$> interactionDataApplicationCommand) interactionGuildId interactionChannelId interactionMember interactionUser interactionToken interactionVersion Nothing
  toInternal InteractionApplicationCommandAutocomplete {..} = InternalInteraction interactionId interactionApplicationId InteractionTypeApplicationCommandAutocomplete (toInternal <$> interactionDataApplicationCommand) interactionGuildId interactionChannelId interactionMember interactionUser interactionToken interactionVersion Nothing
  toInternal (InteractionUnknown i) = i

  fromInternal InternalInteraction {internalInteractionType = InteractionTypePing, ..} = Just $ InteractionPing internalInteractionId internalInteractionApplicationId internalInteractionToken internalInteractionVersion
  fromInternal i@InternalInteraction {internalInteractionType = InteractionTypeMessageComponent, ..} = Just $ fromMaybe (InteractionUnknown i) $ internalInteractionMessage >>= Just . InteractionComponent internalInteractionId internalInteractionApplicationId (internalInteractionData >>= fromInternal) internalInteractionGuildId internalInteractionChannelId internalInteractionMember internalInteractionUser internalInteractionToken internalInteractionVersion
  fromInternal i@InternalInteraction {internalInteractionType=InteractionTypeApplicationCommandAutocomplete,..} = Just $ fromMaybe (InteractionUnknown i) $ process internalInteractionData
    where process Nothing = Just $ InteractionApplicationCommandAutocomplete internalInteractionId internalInteractionApplicationId Nothing internalInteractionGuildId internalInteractionChannelId internalInteractionMember internalInteractionUser internalInteractionToken internalInteractionVersion
          process (Just d) = fromInternal d >>= \d' -> Just $ InteractionApplicationCommandAutocomplete internalInteractionId internalInteractionApplicationId (Just d') internalInteractionGuildId internalInteractionChannelId internalInteractionMember internalInteractionUser internalInteractionToken internalInteractionVersion
  fromInternal i@InternalInteraction {internalInteractionType=InteractionTypeApplicationCommand,..} = Just $ fromMaybe (InteractionUnknown i) $ process internalInteractionData
    where process Nothing = Just $ InteractionApplicationCommand internalInteractionId internalInteractionApplicationId Nothing internalInteractionGuildId internalInteractionChannelId internalInteractionMember internalInteractionUser internalInteractionToken internalInteractionVersion
          process (Just d) = fromInternal d >>= \d' -> Just $ InteractionApplicationCommand internalInteractionId internalInteractionApplicationId (Just d') internalInteractionGuildId internalInteractionChannelId internalInteractionMember internalInteractionUser internalInteractionToken internalInteractionVersion
    -- Just $ InteractionApplicationCommand internalInteractionId internalInteractionApplicationId (internalInteractionType == InteractionTypeApplicationCommandAutocomplete) (internalInteractionData >>= fromInternal) internalInteractionGuildId internalInteractionChannelId internalInteractionMember internalInteractionUser internalInteractionToken internalInteractionVersion

-- instance Internals Interaction InternalInteraction where

-- application command id
-- application command name
-- application command type -- this should be defined in the constructor!
-- resolved data -- this should be formalised and integrated, instead of being
--  left as values
-- options -- only present if type is subcommand or subcommand group

-- | This is the data that is recieved when an interaction occurs.
--
-- https://discord.com/developers/docs/interactions/receiving-and-responding#interaction-object-interaction-structure
data InternalInteraction = InternalInteraction
  { internalInteractionId :: InteractionId,
    internalInteractionApplicationId :: ApplicationId,
    internalInteractionType :: InteractionType, -- referenced as Type in API
    internalInteractionData :: Maybe InternalInteractionData, -- referenced as Data in API
    internalInteractionGuildId :: Maybe GuildId,
    internalInteractionChannelId :: Maybe ChannelId,
    internalInteractionMember :: Maybe GuildMember,
    internalInteractionUser :: Maybe User,
    internalInteractionToken :: InteractionToken,
    internalInteractionVersion :: Int,
    internalInteractionMessage :: Maybe Message
  }
  deriving (Show, Read, Eq)

instance ToJSON InternalInteraction where
  toJSON InternalInteraction {..} =
    object
      [ (name, value)
        | (name, Just value) <-
            [ ("id", toMaybeJSON internalInteractionId),
              ("application_id", toMaybeJSON internalInteractionApplicationId),
              ("type", toMaybeJSON internalInteractionType),
              ("data", toJSON <$> internalInteractionData),
              ("guild_id", toJSON <$> internalInteractionGuildId),
              ("channel_id", toJSON <$> internalInteractionChannelId),
              ("member", toJSON <$> internalInteractionMember),
              ("user", toJSON <$> internalInteractionUser),
              ("token", toMaybeJSON internalInteractionToken),
              ("version", toMaybeJSON internalInteractionVersion),
              ("message", toJSON <$> internalInteractionMessage)
            ]
      ]

instance FromJSON InternalInteraction where
  parseJSON =
    withObject
      "InternalInteraction"
      ( \v ->
          InternalInteraction
            <$> v .: "id"
            <*> v .: "application_id"
            <*> v .: "type"
            <*> v .:? "data"
            <*> v .:? "guild_id"
            <*> v .:? "channel_id"
            <*> v .:? "member"
            <*> v .:? "user"
            <*> v .: "token"
            <*> v .: "version"
            <*> v .:? "message"
      )

-- | This is received if the interaction was a component or application command.
--
-- https://discord.com/developers/docs/interactions/receiving-and-responding#interaction-object-interaction-data-structure
data InternalInteractionData = InternalInteractionData
  { -- | Application command only, id of the invoked command
    internalInteractionDataApplicationCommandId :: Maybe ApplicationCommandId,
    -- | Application command only, name of the invoked command
    internalInteractionDataApplicationCommandName :: Maybe T.Text,
    -- | Application command only, the type of the invoked command
    internalInteractionDataApplicationCommandType :: Maybe ApplicationCommandType,
    -- | Application command only, converted users, roles, channels
    internalInteractionDataResolved :: Maybe ResolvedData,
    -- | Application command only, params and values
    internalInteractionDataOptions :: Maybe [InternalInteractionDataApplicationCommandOption],
    -- | Component only, the unique id
    internalInteractionDataCustomId :: Maybe T.Text,
    -- | Component only, the type of the component
    internalInteractionDataComponentType :: Maybe ComponentType,
    -- | Component only, the selected options if component is the select type
    internalInteractionDataValues :: Maybe [T.Text],
    -- | This is the id of the user or message being targetted by a user command
    -- or a message command
    internalInteractionDataTargetId :: Maybe Snowflake
  }
  deriving (Show, Read, Eq)

instance ToJSON InternalInteractionData where
  toJSON InternalInteractionData {..} =
    object
      [ (name, value)
        | (name, Just value) <-
            [ ("id", toJSON <$> internalInteractionDataApplicationCommandId),
              ("name", toJSON <$> internalInteractionDataApplicationCommandName),
              ("type", toJSON <$> internalInteractionDataApplicationCommandType),
              ("resolved", toJSON <$> internalInteractionDataResolved),
              ("options", toJSON <$> internalInteractionDataOptions),
              ("custom_id", toJSON <$> internalInteractionDataCustomId),
              ("component_type", toJSON <$> internalInteractionDataComponentType),
              ("values", toJSON <$> internalInteractionDataValues),
              ("target_id", toJSON <$> internalInteractionDataTargetId)
            ]
      ]

instance FromJSON InternalInteractionData where
  parseJSON =
    withObject
      "InternalInteractionData"
      ( \v ->
          InternalInteractionData
            <$> v .:? "id"
            <*> v .:? "name"
            <*> v .:? "type"
            <*> v .:? "resolved"
            <*> v .:? "options"
            <*> v .:? "custom_id"
            <*> v .:? "component_type"
            <*> v .:? "values"
            <*> v .:? "target_id"
      )

-- | I'm not sure what this stuff is, so you're on your own.
--
-- It's not worth the time working out how to create this stuff.
-- If you need to extract from these values, check out the link below.
--
-- https://discord.com/developers/docs/interactions/receiving-and-responding#interaction-object-resolved-data-structure
data ResolvedData = ResolvedData
  { resolvedDataUsers :: Maybe Value,
    resolvedDataMembers :: Maybe Value,
    resolvedDataRoles :: Maybe Value,
    resolvedDataChannels :: Maybe Value
  }
  deriving (Show, Read, Eq)

instance ToJSON ResolvedData where
  toJSON ResolvedData {..} =
    object
      [ (name, value)
        | (name, Just value) <-
            [ ("users", resolvedDataUsers),
              ("members", resolvedDataMembers),
              ("roles", resolvedDataRoles),
              ("channels", resolvedDataChannels)
            ]
      ]

instance FromJSON ResolvedData where
  parseJSON =
    withObject
      "ResolvedData"
      ( \v ->
          ResolvedData
            <$> v .:? "users"
            <*> v .:? "members"
            <*> v .:? "roles"
            <*> v .:? "channels"
      )

-- | The application command payload for an interaction.
data InternalInteractionDataApplicationCommandOption = InternalInteractionDataApplicationCommandOption
  { internalInteractionDataApplicationCommandOptionName :: T.Text,
    internalInteractionDataApplicationCommandOptionType :: ApplicationCommandOptionType,
    -- | The value itself. Mutually exclusive with options. Docs are wrong that it's only numbers and strings.
    internalInteractionDataApplicationCommandOptionValue :: Maybe ApplicationCommandInteractionDataValue,
    -- | Only present in group subcommands and subcommands. Mutually exclusive with value.
    internalInteractionDataApplicationCommandOptionOptions :: Maybe [InternalInteractionDataApplicationCommandOption],
    -- | Whether this is the field that the user is currently typing in.
    internalInteractionDataApplicationCommandOptionFocused :: Maybe Bool
  }
  deriving (Show, Read, Eq)

instance ToJSON InternalInteractionDataApplicationCommandOption where
  toJSON InternalInteractionDataApplicationCommandOption {..} =
    object
      [ (name, value)
        | (name, Just value) <-
            [ ("name", toMaybeJSON internalInteractionDataApplicationCommandOptionName),
              ("type", toMaybeJSON internalInteractionDataApplicationCommandOptionType),
              ("value", toJSON <$> internalInteractionDataApplicationCommandOptionValue),
              ("options", toJSON <$> internalInteractionDataApplicationCommandOptionOptions),
              ("focused", toJSON <$> internalInteractionDataApplicationCommandOptionFocused)
            ]
      ]

instance FromJSON InternalInteractionDataApplicationCommandOption where
  parseJSON =
    withObject
      "InternalInteractionDataApplicationCommandOption"
      ( \v ->
          InternalInteractionDataApplicationCommandOption
            <$> v .: "name"
            <*> typeParser v
            <*> (typeParser v >>= \t -> v .:? "value" >>= valueParser t)
            <*> v .:? "options"
            <*> v .:? "focused"
      )
    where
      typeParser v = v .: "type"
      valueParser t (Just v) = parseJSONACIDV t v
      valueParser _ Nothing = return Nothing

-- | The data to respond to an interaction with. Unless specified otherwise, you
-- only have three seconds to reply to an interaction before a failure state is
-- given.
data InteractionResponse = InteractionResponse
  { interactionResponseType :: InteractionCallbackType,
    interactionResponseData :: Maybe InteractionCallbackData
  }
  deriving (Show, Read, Eq)

interactionResponseBasic :: T.Text -> InteractionResponse
interactionResponseBasic t = InteractionResponse InteractionCallbackTypeChannelMessageWithSource (Just . InteractionCallbackDataMessages $ interactionCallbackMessagesBasic t)

instance ToJSON InteractionResponse where
  toJSON InteractionResponse {..} =
    object
      [ (name, value)
        | (name, Just value) <-
            [ ("type", toMaybeJSON interactionResponseType),
              ("data", toJSON <$> interactionResponseData)
            ]
      ]

-- | What's the type of the response?
data InteractionCallbackType
  = -- | Responds to a PING.
    InteractionCallbackTypePong
  | -- | Respond with a message to the interaction
    InteractionCallbackTypeChannelMessageWithSource
  | -- | Respond with a message to the interaction, after a delay. Sending this
    -- back means the interaction token lasts for 15 minutes.
    InteractionCallbackTypeDeferredChannelMessageWithSource
  | -- | For components, edit the original message later.
    InteractionCallbackTypeDeferredUpdateMessage
  | -- | For components, edit the original message.
    InteractionCallbackTypeUpdateMessage
  | -- | Respond to an autocomplete interaction with suggested choices.
    InteractionCallbackTypeApplicationCommandAutocompleteResult
  deriving (Show, Read, Eq, Data)

instance Enum InteractionCallbackType where
  fromEnum InteractionCallbackTypePong = 1
  fromEnum InteractionCallbackTypeChannelMessageWithSource = 4
  fromEnum InteractionCallbackTypeDeferredChannelMessageWithSource = 5
  fromEnum InteractionCallbackTypeDeferredUpdateMessage = 6
  fromEnum InteractionCallbackTypeUpdateMessage = 7
  fromEnum InteractionCallbackTypeApplicationCommandAutocompleteResult = 8
  toEnum a = fromJust $ lookup a table
    where
      table = makeTable InteractionCallbackTypePong

instance ToJSON InteractionCallbackType where
  toJSON = toJSON . fromEnum

-- | Convenience wrapper for two separate types of callback.
data InteractionCallbackData
  = InteractionCallbackDataMessages InteractionCallbackMessages
  | InteractionCallbackDataAutocomplete InteractionCallbackAutocomplete
  deriving (Show, Read, Eq)

instance ToJSON InteractionCallbackData where
  toJSON (InteractionCallbackDataMessages icdm) = toJSON icdm
  toJSON (InteractionCallbackDataAutocomplete icda) = toJSON icda

type InteractionCallbackAutocomplete = [InternalApplicationCommandOptionChoice]

-- | A cut down message structure.
data InteractionCallbackMessages = InteractionCallbackMessages
  { interactionCallbackMessagesTTS :: Maybe Bool,
    interactionCallbackMessagesContent :: Maybe T.Text,
    interactionCallbackMessagesEmbeds :: Maybe [Embed],
    interactionCallbackMessagesAllowedMentions :: Maybe AllowedMentions,
    interactionCallbackMessagesFlags :: Maybe InteractionCallbackDataFlags,
    interactionCallbackMessagesComponents :: Maybe [Component],
    interactionCallbackMessagesAttachments :: Maybe [Attachment]
  }
  deriving (Show, Read, Eq)

interactionCallbackMessagesBasic :: T.Text -> InteractionCallbackMessages
interactionCallbackMessagesBasic t = InteractionCallbackMessages Nothing (Just t) Nothing Nothing Nothing Nothing Nothing

instance ToJSON InteractionCallbackMessages where
  toJSON InteractionCallbackMessages {..} =
    object
      [ (name, value)
        | (name, Just value) <-
            [ ("tts", toJSON <$> interactionCallbackMessagesTTS),
              ("content", toJSON <$> interactionCallbackMessagesContent),
              ("embeds", toJSON <$> interactionCallbackMessagesEmbeds),
              ("allowed_mentions", toJSON <$> interactionCallbackMessagesAllowedMentions),
              ("flags", toJSON <$> interactionCallbackMessagesFlags),
              ("components", toJSON <$> interactionCallbackMessagesComponents),
              ("attachments", toJSON <$> interactionCallbackMessagesAttachments)
            ]
      ]

-- | Types of flags to attach to the interaction message.
--
-- Currently the only flag is EPHERMERAL, which means only the user can see the
-- message.
data InteractionCallbackDataFlag = InteractionCallbackDataFlagEphermeral
  deriving (Show, Read, Eq)

newtype InteractionCallbackDataFlags = InteractionCallbackDataFlags [InteractionCallbackDataFlag]
  deriving (Show, Read, Eq)

instance Enum InteractionCallbackDataFlag where
  fromEnum InteractionCallbackDataFlagEphermeral = 1 `shift` 6
  toEnum i
    | i == 1 `shift` 6 = InteractionCallbackDataFlagEphermeral
    | otherwise = error $ "could not find InteractionCallbackDataFlag `" ++ show i ++ "`"

instance ToJSON InteractionCallbackDataFlags where
  toJSON (InteractionCallbackDataFlags fs) = Number $ fromInteger $ fromIntegral $ foldr (.|.) 0 (fromEnum <$> fs)

data ApplicationCommandInteractionDataValue
  = ApplicationCommandInteractionDataValueString T.Text
  | ApplicationCommandInteractionDataValueInteger Integer
  | ApplicationCommandInteractionDataValueBoolean Bool
  | ApplicationCommandInteractionDataValueUser Snowflake
  | ApplicationCommandInteractionDataValueChannel Snowflake
  | ApplicationCommandInteractionDataValueRole Snowflake
  | ApplicationCommandInteractionDataValueMentionable Snowflake
  | ApplicationCommandInteractionDataValueNumber Scientific
  deriving (Show, Eq, Ord, Read)

getTypeFromACIDV :: ApplicationCommandInteractionDataValue -> ApplicationCommandOptionType
getTypeFromACIDV acidv = case acidv of
  ApplicationCommandInteractionDataValueString _ -> ApplicationCommandOptionTypeString
  ApplicationCommandInteractionDataValueInteger _ -> ApplicationCommandOptionTypeInteger
  ApplicationCommandInteractionDataValueBoolean _ -> ApplicationCommandOptionTypeBoolean
  ApplicationCommandInteractionDataValueUser _ -> ApplicationCommandOptionTypeUser
  ApplicationCommandInteractionDataValueChannel _ -> ApplicationCommandOptionTypeChannel
  ApplicationCommandInteractionDataValueRole _ -> ApplicationCommandOptionTypeRole
  ApplicationCommandInteractionDataValueMentionable _ -> ApplicationCommandOptionTypeMentionable
  ApplicationCommandInteractionDataValueNumber _ -> ApplicationCommandOptionTypeNumber

instance ToJSON ApplicationCommandInteractionDataValue where
  toJSON (ApplicationCommandInteractionDataValueString t) = String t
  toJSON (ApplicationCommandInteractionDataValueNumber t) = Number t
  toJSON (ApplicationCommandInteractionDataValueInteger t) = Number $ fromInteger t
  toJSON (ApplicationCommandInteractionDataValueBoolean t) = Bool t
  toJSON (ApplicationCommandInteractionDataValueUser t) = toJSON t
  toJSON (ApplicationCommandInteractionDataValueChannel t) = toJSON t
  toJSON (ApplicationCommandInteractionDataValueRole t) = toJSON t
  toJSON (ApplicationCommandInteractionDataValueMentionable t) = toJSON t

parseJSONACIDV :: ApplicationCommandOptionType -> Value -> Parser (Maybe ApplicationCommandInteractionDataValue)
parseJSONACIDV ApplicationCommandOptionTypeString (String t) = return $ return (ApplicationCommandInteractionDataValueString t)
parseJSONACIDV ApplicationCommandOptionTypeInteger n = Just . ApplicationCommandInteractionDataValueInteger <$> parseJSON n
parseJSONACIDV ApplicationCommandOptionTypeNumber (Number t) = return $ return (ApplicationCommandInteractionDataValueNumber t)
parseJSONACIDV ApplicationCommandOptionTypeBoolean (Bool t) = return $ return (ApplicationCommandInteractionDataValueBoolean t)
parseJSONACIDV ApplicationCommandOptionTypeUser t = Just . ApplicationCommandInteractionDataValueUser <$> parseJSON t
parseJSONACIDV ApplicationCommandOptionTypeChannel t = Just . ApplicationCommandInteractionDataValueChannel <$> parseJSON t
parseJSONACIDV ApplicationCommandOptionTypeRole t = Just . ApplicationCommandInteractionDataValueRole <$> parseJSON t
parseJSONACIDV ApplicationCommandOptionTypeMentionable t = Just . ApplicationCommandInteractionDataValueMentionable <$> parseJSON t
parseJSONACIDV t v = fail $ "could not parse type " ++ show t ++ " from " ++ show v
