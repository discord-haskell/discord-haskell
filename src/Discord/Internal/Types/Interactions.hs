{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Discord.Internal.Types.Interactions
  ( Interaction (..),
    InteractionId,
    InteractionToken,
    InteractionType (..),
    InteractionData (..),
    ResolvedData (..),
    ApplicationCommandInteractionDataOption (..),
    InteractionResponse (..),
    InteractionCallbackType (..),
    InteractionCallbackData (..),
    InteractionCallbackAutocomplete,
    InteractionCallbackMessages (..),
    InteractionCallbackDataFlags (..),
    InteractionCallbackDataFlag (..),
  )
where

import Data.Aeson
import Data.Bits (Bits (shift, (.|.)))
import Data.Data ( Data(dataTypeOf), dataTypeConstrs, fromConstr )
import Data.Default (Default (..))
import Data.Maybe (fromJust)
import Data.Text (Text)
import Discord.Internal.Types.ApplicationCommands
import Discord.Internal.Types.Channel (AllowedMentions, Attachment, Message)
import Discord.Internal.Types.Embed (Embed)
import Discord.Internal.Types.Guild (GuildMember)
import Discord.Internal.Types.Prelude (ApplicationId, ChannelId, GuildId, Snowflake)
import Discord.Internal.Types.User (User)

toMaybeJSON :: (ToJSON a) => a -> Maybe Value
toMaybeJSON = return . toJSON

makeTable :: (Data t, Enum t) => t -> [(Int, t)]
makeTable t = map (\cData -> let c = fromConstr cData in (fromEnum c, c)) (dataTypeConstrs $ dataTypeOf t)

type InteractionId = Snowflake

type InteractionToken = Text

-- | This is the data that is recieved when an interaction occurs.
--
-- https://discord.com/developers/docs/interactions/receiving-and-responding#interaction-object-interaction-structure
data Interaction = Interaction
  { interactionId :: InteractionId,
    interactionApplicationId :: ApplicationId,
    interactionType :: InteractionType, -- referenced as Type in API
    interactionData :: Maybe InteractionData, -- referenced as Data in API
    interactionGuildId :: Maybe GuildId,
    interactionChannelId :: Maybe ChannelId,
    interactionMember :: Maybe GuildMember,
    interactionUser :: Maybe User,
    interactionToken :: InteractionToken,
    interactionVersion :: Int,
    interactionMessage :: Maybe Message
  }
  deriving (Show, Read, Eq)

instance ToJSON Interaction where
  toJSON Interaction {..} =
    object
      [ (name, value)
        | (name, Just value) <-
            [ ("id", toMaybeJSON interactionId),
              ("application_id", toMaybeJSON interactionApplicationId),
              ("type", toMaybeJSON interactionType),
              ("data", toJSON <$> interactionData),
              ("guild_id", toJSON <$> interactionGuildId),
              ("channel_id", toJSON <$> interactionChannelId),
              ("member", toJSON <$> interactionMember),
              ("user", toJSON <$> interactionUser),
              ("token", toMaybeJSON interactionToken),
              ("version", toMaybeJSON interactionVersion),
              ("message", toJSON <$> interactionMessage)
            ]
      ]

instance FromJSON Interaction where
  parseJSON =
    withObject
      "Interaction"
      ( \v ->
          Interaction
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

-- | What type of interaction has a user requested? Each requires its own type
-- of response.
data InteractionType = PING | APPLICATION_COMMAND | MESSAGE_COMPONENT | APPLICATION_COMMAND_AUTOCOMPLETE
  deriving (Show, Read, Data, Eq)

instance Enum InteractionType where
  fromEnum PING = 1
  fromEnum APPLICATION_COMMAND = 2
  fromEnum MESSAGE_COMPONENT = 3
  fromEnum APPLICATION_COMMAND_AUTOCOMPLETE = 4
  toEnum a = fromJust $ lookup a table
    where
      table = makeTable PING

instance ToJSON InteractionType where
  toJSON = toJSON . fromEnum

instance FromJSON InteractionType where
  parseJSON = withScientific "InteractionType" (return . toEnum . round)

-- | This is received if the interaction was a component or application command.
--
-- Components are currently not supported.
--
-- https://discord.com/developers/docs/interactions/receiving-and-responding#interaction-object-interaction-data-structure
data InteractionData = InteractionData
  { interactionDataApplicationCommandId :: ApplicationCommandId,
    interactionDataApplicationCommandName :: String,
    interactionDataApplicationCommandType :: ApplicationCommandType,
    interactionDataResolved :: Maybe ResolvedData,
    interactionDataOptions :: Maybe [ApplicationCommandInteractionDataOption],
    -- , interactionDataCustomId :: Maybe String
    -- , interactionDataComponentType :: Maybe Int -- ^ this is likely to change in future if and when it's needed

    -- | this is the id of the user or message being targetteed by a user command or a message command
    interactionDataTargetId :: Maybe Snowflake
  }
  deriving (Show, Read, Eq)

instance ToJSON InteractionData where
  toJSON InteractionData {..} =
    object
      [ (name, value)
        | (name, Just value) <-
            [ ("id", toMaybeJSON interactionDataApplicationCommandId),
              ("name", toMaybeJSON interactionDataApplicationCommandName),
              ("type", toMaybeJSON interactionDataApplicationCommandType),
              ("resolved", toJSON <$> interactionDataResolved),
              ("options", toJSON <$> interactionDataOptions),
              -- missing info relevant for components
              --   , ("custom_id", toJSON <$> interactionDataCustomId)
              --   , ("component_type", toJSON <$> interactionDataComponentType)
              ("target_id", toJSON <$> interactionDataTargetId)
            ]
      ]

instance FromJSON InteractionData where
  parseJSON =
    withObject
      "InteractionData"
      ( \v ->
          InteractionData
            <$> v .: "id"
            <*> v .: "name"
            <*> v .: "type"
            <*> v .:? "resolved"
            <*> v .:? "options"
            -- <*> v .:? "custom_id"
            -- <*> v .:? "component_type"
            -- <*> v .:? "values"
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
data ApplicationCommandInteractionDataOption = ApplicationCommandInteractionDataOption
  { applicationCommandInteractionDataOptionName :: String,
    applicationCommandInteractionDataOptionType :: ApplicationCommandOptionType,
    -- | The value itself. Mutually exclusive with options.
    applicationCommandInteractionDataOptionValue :: Maybe StringNumber,
    -- | Only present in group subcommands and subcommands. Mutually exclusive with value.
    applicationCommandInteractionDataOptionOptions :: Maybe [ApplicationCommandInteractionDataOption],
    -- | Whether this is the field that the user is currently typing in.
    applicationCommandInteractionDataOptionFocused :: Maybe Bool
  }
  deriving (Show, Read, Eq)

instance ToJSON ApplicationCommandInteractionDataOption where
  toJSON ApplicationCommandInteractionDataOption {..} =
    object
      [ (name, value)
        | (name, Just value) <-
            [ ("name", toMaybeJSON applicationCommandInteractionDataOptionName),
              ("type", toMaybeJSON applicationCommandInteractionDataOptionType),
              ("value", toJSON <$> applicationCommandInteractionDataOptionValue),
              ("options", toJSON <$> applicationCommandInteractionDataOptionOptions),
              ("focused", toJSON <$> applicationCommandInteractionDataOptionFocused)
            ]
      ]

instance FromJSON ApplicationCommandInteractionDataOption where
  parseJSON =
    withObject
      "ApplicationCommandInteractionDataOption"
      ( \v ->
          ApplicationCommandInteractionDataOption
            <$> v .: "name"
            <*> v .: "type"
            <*> v .:? "value"
            <*> v .:? "options"
            <*> v .:? "focused"
      )

-- | The data to respond to an interaction with. Unless specified otherwise, you
-- only have three seconds to reply to an interaction before a failure state is
-- given.
data InteractionResponse = InteractionResponse
  { interactionResponseType :: InteractionCallbackType,
    interactionResponseData :: Maybe InteractionCallbackData
  }
  deriving (Show, Read, Eq)

instance Default InteractionResponse where
  def = InteractionResponse CHANNEL_MESSAGE_WITH_SOURCE Nothing

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
    PONG
  | -- | Respond with a message to the interaction
    CHANNEL_MESSAGE_WITH_SOURCE
  | -- | Respond with a message to the interaction, after a delay. Sending this
    -- back means the interaction token lasts for 15 minutes.
    DEFERRED_CHANNEL_MESSAGE_WITH_SOURCE
  | -- | For components, edit the original message later.
    DEFERRED_UPDATE_MESSAGE
  | -- | For components, edit the original message.
    UPDATE_MESSAGE
  | -- | Respond to an autocomplete interaction with suggested choices.
    APPLICATION_COMMAND_AUTOCOMPLETE_RESULT
  deriving (Show, Read, Eq, Data)

instance Enum InteractionCallbackType where
  fromEnum PONG = 1
  fromEnum CHANNEL_MESSAGE_WITH_SOURCE = 4
  fromEnum DEFERRED_CHANNEL_MESSAGE_WITH_SOURCE = 5
  fromEnum DEFERRED_UPDATE_MESSAGE = 6
  fromEnum UPDATE_MESSAGE = 6
  fromEnum APPLICATION_COMMAND_AUTOCOMPLETE_RESULT = 6
  toEnum a = fromJust $ lookup a table
    where
      table = makeTable PONG

instance ToJSON InteractionCallbackType where
  toJSON = toJSON . fromEnum

-- | Convenience wrapper for two separate types of callback.
data InteractionCallbackData = ICDM InteractionCallbackMessages | ICDA InteractionCallbackAutocomplete
  deriving (Show, Read, Eq)

instance ToJSON InteractionCallbackData where
  toJSON (ICDM icdm) = toJSON icdm
  toJSON (ICDA icda) = toJSON icda

type InteractionCallbackAutocomplete = [ApplicationCommandOptionChoice]

-- | A cut down message structure.
data InteractionCallbackMessages = InteractionCallbackMessages
  { interactionCallbackDataMessagesTTS :: Maybe Bool,
    interactionCallbackDataMessagesContent :: Maybe Text,
    interactionCallbackDataMessagesEmbeds :: Maybe [Embed],
    interactionCallbackDataMessagesAllowedMentions :: Maybe [AllowedMentions],
    interactionCallbackDataMessagesFlags :: Maybe InteractionCallbackDataFlags,
    -- missing components
    interactionCallbackDataMessagesAttachments :: Maybe [Attachment]
  }
  deriving (Show, Read, Eq)

instance Default InteractionCallbackMessages where
  def = InteractionCallbackMessages Nothing Nothing Nothing Nothing Nothing Nothing

instance ToJSON InteractionCallbackMessages where
  toJSON InteractionCallbackMessages {..} =
    object
      [ (name, value)
        | (name, Just value) <-
            [ ("tts", toJSON <$> interactionCallbackDataMessagesTTS),
              ("content", toJSON <$> interactionCallbackDataMessagesContent),
              ("embeds", toJSON <$> interactionCallbackDataMessagesEmbeds),
              ("allowed_mentions", toJSON <$> interactionCallbackDataMessagesAllowedMentions),
              ("flags", toJSON <$> interactionCallbackDataMessagesFlags),
              ("attachments", toJSON <$> interactionCallbackDataMessagesAttachments)
            ]
      ]

-- | Types of flags to attack to the message.
--
-- Currently the only flag is EPHERMERAL, which means only the user can see the
-- message.
data InteractionCallbackDataFlag = EPHERMERAL
  deriving (Show, Read, Eq)

newtype InteractionCallbackDataFlags = InteractionCallbackDataFlags [InteractionCallbackDataFlag]
  deriving (Show, Read, Eq)

instance Enum InteractionCallbackDataFlag where
  fromEnum EPHERMERAL = 1 `shift` 6
  toEnum i
    | i == 1 `shift` 6 = EPHERMERAL
    | otherwise = error $ "could not find InteractionCallbackDataFlag `" ++ show i ++ "`"

instance ToJSON InteractionCallbackDataFlags where
  toJSON (InteractionCallbackDataFlags fs) = Number $ fromInteger $ fromIntegral $ foldr (.|.) 0 (fromEnum <$> fs)
