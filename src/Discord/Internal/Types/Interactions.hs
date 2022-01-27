{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module Discord.Internal.Types.Interactions
  ( Interaction (..),
    InteractionDataComponent (..),
    InteractionDataApplicationCommand (..),
    InteractionDataApplicationCommandOptions (..),
    InteractionDataApplicationCommandOptionSubcommandOrGroup (..),
    InteractionDataApplicationCommandOptionSubcommand (..),
    InteractionDataApplicationCommandOptionValue (..),
    InteractionToken,
    ResolvedData (..),
    MemberOrUser (..),
    InteractionResponse (..),
    interactionResponseBasic,
    InteractionResponseAutocomplete (..),
    InteractionResponseMessage (..),
    interactionResponseMessageBasic,
    InteractionResponseMessageFlags (..),
    InteractionResponseMessageFlag (..),
  )
where

import Control.Applicative (Alternative ((<|>)))
import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Bits (Bits (shift, (.|.)))
import Data.Foldable (Foldable (toList))
import Data.Scientific (Scientific)
import qualified Data.Text as T
import Discord.Internal.Types.ApplicationCommands (Choice)
import Discord.Internal.Types.Channel (AllowedMentions, Attachment, Message)
import Discord.Internal.Types.Components (ComponentActionRow)
import Discord.Internal.Types.Embed (CreateEmbed, createEmbed)
import Discord.Internal.Types.Prelude (ApplicationCommandId, ApplicationId, ChannelId, GuildId, InteractionId, InteractionToken, MessageId, RoleId, Snowflake, UserId)
import Discord.Internal.Types.User (GuildMember, User)

-- | An interaction received from discord.
data Interaction
  = InteractionComponent
      { -- | The id of this interaction.
        interactionId :: InteractionId,
        -- | The id of the application that this interaction belongs to.
        interactionApplicationId :: ApplicationId,
        -- | The data for this interaction.
        interactionDataComponent :: InteractionDataComponent,
        -- | What guild this interaction comes from.
        interactionGuildId :: Maybe GuildId,
        -- | What channel this interaction comes from.
        interactionChannelId :: Maybe ChannelId,
        -- | What user/member this interaction comes from.
        interactionUser :: MemberOrUser,
        -- | The unique token that represents this interaction.
        interactionToken :: InteractionToken,
        -- | What version of interaction is this (always 1).
        interactionVersion :: Int,
        -- | What message is associated with this interaction.
        interactionMessage :: Message,
        -- | The invoking user's preferred locale.
        interactionLocale :: T.Text,
        -- | The invoking guild's preferred locale.
        interactionGuildLocale :: Maybe T.Text
      }
  | InteractionPing
      { -- | The id of this interaction.
        interactionId :: InteractionId,
        -- | The id of the application that this interaction belongs to.
        interactionApplicationId :: ApplicationId,
        -- | The unique token that represents this interaction.
        interactionToken :: InteractionToken,
        -- | What version of interaction is this (always 1).
        interactionVersion :: Int
      }
  | InteractionApplicationCommand
      { -- | The id of this interaction.
        interactionId :: InteractionId,
        -- | The id of the application that this interaction belongs to.
        interactionApplicationId :: ApplicationId,
        -- | The data for this interaction.
        interactionDataApplicationCommand :: InteractionDataApplicationCommand,
        -- | What guild this interaction comes from.
        interactionGuildId :: Maybe GuildId,
        -- | What channel this interaction comes from.
        interactionChannelId :: Maybe ChannelId,
        -- | What user/member this interaction comes from.
        interactionUser :: MemberOrUser,
        -- | The unique token that represents this interaction.
        interactionToken :: InteractionToken,
        -- | What version of interaction is this (always 1).
        interactionVersion :: Int,
        -- | The invoking user's preferred locale.
        interactionLocale :: T.Text,
        -- | The invoking guild's preferred locale.
        interactionGuildLocale :: Maybe T.Text
      }
  | InteractionApplicationCommandAutocomplete
      { -- | The id of this interaction.
        interactionId :: InteractionId,
        -- | The id of the application that this interaction belongs to.
        interactionApplicationId :: ApplicationId,
        -- | The data for this interaction.
        interactionDataApplicationCommand :: InteractionDataApplicationCommand,
        -- | What guild this interaction comes from.
        interactionGuildId :: Maybe GuildId,
        -- | What channel this interaction comes from.
        interactionChannelId :: Maybe ChannelId,
        -- | What user/member this interaction comes from.
        interactionUser :: MemberOrUser,
        -- | The unique token that represents this interaction.
        interactionToken :: InteractionToken,
        -- | What version of interaction is this (always 1).
        interactionVersion :: Int,
        -- | The invoking user's preferred locale.
        interactionLocale :: T.Text,
        -- | The invoking guild's preferred locale.
        interactionGuildLocale :: Maybe T.Text
      }
  deriving (Show, Eq)

instance FromJSON Interaction where
  parseJSON =
    withObject
      "Interaction"
      ( \v -> do
          iid <- v .: "id"
          aid <- v .: "application_id"
          gid <- v .:? "guild_id"
          cid <- v .:? "channel_id"
          tok <- v .: "token"
          version <- v .: "version"
          glocale <- v .:? "guild_locale"
          t <- v .: "type" :: Parser Int
          case t of
            1 -> return $ InteractionPing iid aid tok version
            2 ->
              InteractionApplicationCommand iid aid
                <$> v .: "data"
                <*> return gid
                <*> return cid
                <*> parseJSON (Object v)
                <*> return tok
                <*> return version
                <*> v .: "locale"
                <*> return glocale
            3 ->
              InteractionComponent iid aid
                <$> v .: "data"
                <*> return gid
                <*> return cid
                <*> parseJSON (Object v)
                <*> return tok
                <*> return version
                <*> v .: "message"
                <*> v .: "locale"
                <*> return glocale
            4 ->
              InteractionApplicationCommandAutocomplete iid aid
                <$> v .: "data"
                <*> return gid
                <*> return cid
                <*> parseJSON (Object v)
                <*> return tok
                <*> return version
                <*> v .: "locale"
                <*> return glocale
            _ -> fail "unknown interaction type"
      )

newtype MemberOrUser = MemberOrUser (Either GuildMember User) deriving (Show, Eq)

instance {-# OVERLAPPING #-} FromJSON MemberOrUser where
  parseJSON =
    withObject
      "MemberOrUser"
      ( \v -> MemberOrUser <$> ((Left <$> v .: "member") <|> (Right <$> v .: "user"))
      )

data InteractionDataComponent
  = InteractionDataComponentButton
      { -- | The unique id of the component (up to 100 characters).
        interactionDataComponentCustomId :: T.Text
      }
  | InteractionDataComponentSelectMenu
      { -- | The unique id of the component (up to 100 characters).
        interactionDataComponentCustomId :: T.Text,
        -- | Values for the select menu.
        interactionDataComponentValues :: [T.Text]
      }
  deriving (Show, Read, Eq)

instance FromJSON InteractionDataComponent where
  parseJSON =
    withObject
      "InteractionDataComponent"
      ( \v -> do
          cid <- v .: "custom_id"
          t <- v .: "component_type" :: Parser Int
          case t of
            2 -> return $ InteractionDataComponentButton cid
            3 ->
              InteractionDataComponentSelectMenu cid
                <$> v .: "values"
            _ -> fail "unknown interaction data component type"
      )

data InteractionDataApplicationCommand
  = InteractionDataApplicationCommandUser
      { -- | Id of the invoked command.
        interactionDataApplicationCommandId :: ApplicationCommandId,
        -- | Name of the invoked command.
        interactionDataApplicationCommandName :: T.Text,
        -- | The resolved data in the command.
        interactionDataApplicationCommandResolvedData :: Maybe ResolvedData,
        -- | The id of the user that is the target.
        interactionDataApplicationCommandTargetId :: UserId
      }
  | InteractionDataApplicationCommandMessage
      { -- | Id of the invoked command.
        interactionDataApplicationCommandId :: ApplicationCommandId,
        -- | Name of the invoked command.
        interactionDataApplicationCommandName :: T.Text,
        -- | The resolved data in the command.
        interactionDataApplicationCommandResolvedData :: Maybe ResolvedData,
        -- | The id of the message that is the target.
        interactionDataApplicationCommandTargetId :: MessageId
      }
  | InteractionDataApplicationCommandChatInput
      { -- | Id of the invoked command.
        interactionDataApplicationCommandId :: ApplicationCommandId,
        -- | Name of the invoked command.
        interactionDataApplicationCommandName :: T.Text,
        -- | The resolved data in the command.
        interactionDataApplicationCommandResolvedData :: Maybe ResolvedData,
        -- | The options of the application command.
        interactionDataApplicationCommandOptions :: Maybe InteractionDataApplicationCommandOptions
      }
  deriving (Show, Read, Eq)

instance FromJSON InteractionDataApplicationCommand where
  parseJSON =
    withObject
      "InteractionDataApplicationCommand"
      ( \v -> do
          aci <- v .: "id"
          name <- v .: "name"
          rd <- v .:? "resolved_data"
          t <- v .: "type" :: Parser Int
          case t of
            1 ->
              InteractionDataApplicationCommandChatInput aci name rd
                <$> v .:? "options"
            2 ->
              InteractionDataApplicationCommandUser aci name rd
                <$> v .: "target_id"
            3 ->
              InteractionDataApplicationCommandMessage aci name rd
                <$> v .: "target_id"
            _ -> fail "unknown interaction data component type"
      )

-- | Either subcommands and groups, or values.
data InteractionDataApplicationCommandOptions
  = InteractionDataApplicationCommandOptionsSubcommands [InteractionDataApplicationCommandOptionSubcommandOrGroup]
  | InteractionDataApplicationCommandOptionsValues [InteractionDataApplicationCommandOptionValue]
  deriving (Show, Read, Eq)

instance FromJSON InteractionDataApplicationCommandOptions where
  parseJSON =
    withArray
      "InteractionDataApplicationCommandOptions"
      ( \a -> do
          let a' = toList a
          case a' of
            [] -> return $ InteractionDataApplicationCommandOptionsValues []
            (v' : _) ->
              withObject
                "InteractionDataApplicationCommandOptions item"
                ( \v -> do
                    t <- v .: "type" :: Parser Int
                    if t == 1 || t == 2
                      then InteractionDataApplicationCommandOptionsSubcommands <$> mapM parseJSON a'
                      else InteractionDataApplicationCommandOptionsValues <$> mapM parseJSON a'
                )
                v'
      )

-- | Either a subcommand group or a subcommand.
data InteractionDataApplicationCommandOptionSubcommandOrGroup
  = InteractionDataApplicationCommandOptionSubcommandGroup
      { interactionDataApplicationCommandOptionSubcommandGroupName :: T.Text,
        interactionDataApplicationCommandOptionSubcommandGroupOptions :: [InteractionDataApplicationCommandOptionSubcommand],
        interactionDataApplicationCommandOptionSubcommandGroupFocused :: Bool
      }
  | InteractionDataApplicationCommandOptionSubcommandOrGroupSubcommand InteractionDataApplicationCommandOptionSubcommand
  deriving (Show, Read, Eq)

instance FromJSON InteractionDataApplicationCommandOptionSubcommandOrGroup where
  parseJSON =
    withObject
      "InteractionDataApplicationCommandOptionSubcommandOrGroup"
      ( \v -> do
          t <- v .: "type" :: Parser Int
          case t of
            2 ->
              InteractionDataApplicationCommandOptionSubcommandGroup
                <$> v .: "name"
                <*> v .: "options"
                <*> v .:? "focused" .!= False
            1 -> InteractionDataApplicationCommandOptionSubcommandOrGroupSubcommand <$> parseJSON (Object v)
            _ -> fail "unexpected subcommand group type"
      )

-- | Data for a single subcommand.
data InteractionDataApplicationCommandOptionSubcommand = InteractionDataApplicationCommandOptionSubcommand
  { interactionDataApplicationCommandOptionSubcommandName :: T.Text,
    interactionDataApplicationCommandOptionSubcommandOptions :: [InteractionDataApplicationCommandOptionValue],
    interactionDataApplicationCommandOptionSubcommandFocused :: Bool
  }
  deriving (Show, Read, Eq)

instance FromJSON InteractionDataApplicationCommandOptionSubcommand where
  parseJSON =
    withObject
      "InteractionDataApplicationCommandOptionSubcommand"
      ( \v -> do
          t <- v .: "type" :: Parser Int
          case t of
            1 ->
              InteractionDataApplicationCommandOptionSubcommand
                <$> v .: "name"
                <*> v .: "options"
                <*> v .:? "focused" .!= False
            _ -> fail "unexpected subcommand type"
      )

-- | Data for a single value.
data InteractionDataApplicationCommandOptionValue
  = InteractionDataApplicationCommandOptionValueString
      { interactionDataApplicationCommandOptionValueName :: T.Text,
        interactionDataApplicationCommandOptionValueStringValue :: Either T.Text T.Text
      }
  | InteractionDataApplicationCommandOptionValueInteger
      { interactionDataApplicationCommandOptionValueName :: T.Text,
        interactionDataApplicationCommandOptionValueIntegerValue :: Either T.Text Integer
      }
  | InteractionDataApplicationCommandOptionValueBoolean
      { interactionDataApplicationCommandOptionValueName :: T.Text,
        interactionDataApplicationCommandOptionValueBooleanValue :: Bool
      }
  | InteractionDataApplicationCommandOptionValueUser
      { interactionDataApplicationCommandOptionValueName :: T.Text,
        interactionDataApplicationCommandOptionValueUserValue :: UserId
      }
  | InteractionDataApplicationCommandOptionValueChannel
      { interactionDataApplicationCommandOptionValueName :: T.Text,
        interactionDataApplicationCommandOptionValueChannelValue :: ChannelId
      }
  | InteractionDataApplicationCommandOptionValueRole
      { interactionDataApplicationCommandOptionValueName :: T.Text,
        interactionDataApplicationCommandOptionValueRoleValue :: RoleId
      }
  | InteractionDataApplicationCommandOptionValueMentionable
      { interactionDataApplicationCommandOptionValueName :: T.Text,
        interactionDataApplicationCommandOptionValueMentionableValue :: Snowflake
      }
  | InteractionDataApplicationCommandOptionValueNumber
      { interactionDataApplicationCommandOptionValueName :: T.Text,
        interactionDataApplicationCommandOptionValueNumberValue :: Either T.Text Scientific
      }
  deriving (Show, Read, Eq)

instance FromJSON InteractionDataApplicationCommandOptionValue where
  parseJSON =
    withObject
      "InteractionDataApplicationCommandOptionValue"
      ( \v -> do
          name <- v .: "name"
          focused <- v .:? "focused" .!= False
          t <- v .: "type" :: Parser Int
          case t of
            3 ->
              InteractionDataApplicationCommandOptionValueString name
                <$> parseValue v focused
            4 ->
              InteractionDataApplicationCommandOptionValueInteger name
                <$> parseValue v focused
            10 ->
              InteractionDataApplicationCommandOptionValueNumber name
                <$> parseValue v focused
            5 ->
              InteractionDataApplicationCommandOptionValueBoolean name
                <$> v .: "value"
            6 ->
              InteractionDataApplicationCommandOptionValueUser name
                <$> v .: "value"
            7 ->
              InteractionDataApplicationCommandOptionValueChannel name
                <$> v .: "value"
            8 ->
              InteractionDataApplicationCommandOptionValueRole name
                <$> v .: "value"
            9 ->
              InteractionDataApplicationCommandOptionValueMentionable name
                <$> v .: "value"
            _ -> fail $ "unexpected interaction data application command option value type: " ++ show t
      )

parseValue :: (FromJSON a) => Object -> Bool -> Parser (Either T.Text a)
parseValue o True = Left <$> o .: "value"
parseValue o False = Right <$> o .: "value"

-- resolved data -- this should be formalised and integrated, instead of being
--  left as values

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

-- | The application command payload for an i

-- | The data to respond to an interaction with. Unless specified otherwise, you
-- only have three seconds to reply to an interaction before a failure state is
-- given.
data InteractionResponse
  = -- | ACK a Ping
    InteractionResponsePong
  | -- | Respond to an interaction with a message
    InteractionResponseChannelMessage InteractionResponseMessage
  | -- | ACK an interaction and edit a response later (use `CreateFollowupInteractionMessage` and `InteractionResponseMessage` to do so). User sees loading state.
    InteractionResponseDeferChannelMessage
  | -- | for components, ACK an interaction and edit the original message later; the user does not see a loading state.
    InteractionResponseDeferUpdateMessage
  | -- | for components, edit the message the component was attached to
    InteractionResponseUpdateMessage InteractionResponseMessage
  | -- | respond to an autocomplete interaction with suggested choices
    InteractionResponseAutocompleteResult InteractionResponseAutocomplete

-- | A basic interaction response, sending back the given text.
interactionResponseBasic :: T.Text -> InteractionResponse
interactionResponseBasic t = InteractionResponseChannelMessage (interactionResponseMessageBasic t)

instance ToJSON InteractionResponse where
  toJSON InteractionResponsePong = object [("type", Number 1)]
  toJSON InteractionResponseDeferChannelMessage = object [("type", Number 5)]
  toJSON InteractionResponseDeferUpdateMessage = object [("type", Number 6)]
  toJSON (InteractionResponseChannelMessage ms) = object [("type", Number 4), ("data", toJSON ms)]
  toJSON (InteractionResponseUpdateMessage ms) = object [("type", Number 7), ("data", toJSON ms)]
  toJSON (InteractionResponseAutocompleteResult ms) = object [("type", Number 8), ("data", toJSON ms)]

data InteractionResponseAutocomplete = InteractionResponseAutocompleteString [Choice T.Text] | InteractionResponseAutocompleteInteger [Choice Integer] | InteractionResponseAutocompleteNumber [Choice Scientific]
  deriving (Show, Eq)

instance ToJSON InteractionResponseAutocomplete where
  toJSON (InteractionResponseAutocompleteString cs) = object [("choices", toJSON cs)]
  toJSON (InteractionResponseAutocompleteInteger cs) = object [("choices", toJSON cs)]
  toJSON (InteractionResponseAutocompleteNumber cs) = object [("choices", toJSON cs)]

-- | A cut down message structure.
data InteractionResponseMessage = InteractionResponseMessage
  { interactionResponseMessageTTS :: Maybe Bool,
    interactionResponseMessageContent :: Maybe T.Text,
    interactionResponseMessageEmbeds :: Maybe [CreateEmbed],
    interactionResponseMessageAllowedMentions :: Maybe AllowedMentions,
    interactionResponseMessageFlags :: Maybe InteractionResponseMessageFlags,
    interactionResponseMessageComponents :: Maybe [ComponentActionRow],
    interactionResponseMessageAttachments :: Maybe [Attachment]
  }
  deriving (Show, Eq)

-- | A basic interaction response, sending back the given text. This is
-- effectively a helper function.
interactionResponseMessageBasic :: T.Text -> InteractionResponseMessage
interactionResponseMessageBasic t = InteractionResponseMessage Nothing (Just t) Nothing Nothing Nothing Nothing Nothing

instance ToJSON InteractionResponseMessage where
  toJSON InteractionResponseMessage {..} =
    object
      [ (name, value)
        | (name, Just value) <-
            [ ("tts", toJSON <$> interactionResponseMessageTTS),
              ("content", toJSON <$> interactionResponseMessageContent),
              ("embeds", toJSON . (createEmbed <$>) <$> interactionResponseMessageEmbeds),
              ("allowed_mentions", toJSON <$> interactionResponseMessageAllowedMentions),
              ("flags", toJSON <$> interactionResponseMessageFlags),
              ("components", toJSON <$> interactionResponseMessageComponents),
              ("attachments", toJSON <$> interactionResponseMessageAttachments)
            ]
      ]

-- | Types of flags to attach to the interaction message.
--
-- Currently the only flag is EPHERMERAL, which means only the user can see the
-- message.
data InteractionResponseMessageFlag = InteractionResponseMessageFlagEphermeral
  deriving (Show, Read, Eq)

newtype InteractionResponseMessageFlags = InteractionResponseMessageFlags [InteractionResponseMessageFlag]
  deriving (Show, Read, Eq)

instance Enum InteractionResponseMessageFlag where
  fromEnum InteractionResponseMessageFlagEphermeral = 1 `shift` 6
  toEnum i
    | i == 1 `shift` 6 = InteractionResponseMessageFlagEphermeral
    | otherwise = error $ "could not find InteractionCallbackDataFlag `" ++ show i ++ "`"

instance ToJSON InteractionResponseMessageFlags where
  toJSON (InteractionResponseMessageFlags fs) = Number $ fromInteger $ fromIntegral $ foldr (.|.) 0 (fromEnum <$> fs)
