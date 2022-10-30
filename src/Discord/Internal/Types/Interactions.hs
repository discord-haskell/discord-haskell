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
    ComponentData (..),
    ApplicationCommandData (..),
    OptionsData (..),
    OptionDataSubcommandOrGroup (..),
    OptionDataSubcommand (..),
    OptionDataValue (..),
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
    InteractionResponseModalData (..),
  )
where

import Control.Applicative (Alternative ((<|>)))
import Control.Monad (join)
import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Bits (Bits (shift, (.|.)))
import Data.Foldable (Foldable (toList))
import qualified Data.Text as T
import Discord.Internal.Types.ApplicationCommands (Choice, Number)
import Discord.Internal.Types.Channel (AllowedMentions, Attachment, Message)
import Discord.Internal.Types.Components (ActionRow, TextInput)
import Discord.Internal.Types.Embed (CreateEmbed, createEmbed)
import Discord.Internal.Types.Prelude (ApplicationCommandId, ApplicationId, ChannelId, GuildId, InteractionId, InteractionToken, MessageId, RoleId, Snowflake, UserId, objectFromMaybes, (.=?))
import Discord.Internal.Types.User (GuildMember, User)

-- | An interaction received from discord.
data Interaction
  = InteractionComponent
      { -- | The id of this interaction.
        interactionId :: InteractionId,
        -- | The id of the application that this interaction belongs to.
        interactionApplicationId :: ApplicationId,
        -- | The data for this interaction.
        componentData :: ComponentData,
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
        -- | What permissions does the app or bot have within the sent channel.
        interactionPermissions :: Maybe T.Text,
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
        interactionVersion :: Int,
        -- | What permissions does the app or bot have within the sent channel.
        interactionPermissions :: Maybe T.Text
      }
  | InteractionApplicationCommand
      { -- | The id of this interaction.
        interactionId :: InteractionId,
        -- | The id of the application that this interaction belongs to.
        interactionApplicationId :: ApplicationId,
        -- | The data for this interaction.
        applicationCommandData :: ApplicationCommandData,
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
        -- | What permissions does the app or bot have within the sent channel.
        interactionPermissions :: Maybe T.Text,
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
        applicationCommandData :: ApplicationCommandData,
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
        -- | What permissions does the app or bot have within the sent channel.
        interactionPermissions :: Maybe T.Text,
        -- | The invoking user's preferred locale.
        interactionLocale :: T.Text,
        -- | The invoking guild's preferred locale.
        interactionGuildLocale :: Maybe T.Text
      }
  | InteractionModalSubmit
      { -- | The id of this interaction.
        interactionId :: InteractionId,
        -- | The id of the application that this interaction belongs to.
        interactionApplicationId :: ApplicationId,
        -- | The data for this interaction.
        modalData :: ModalData,
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
        -- | What permissions does the app or bot have within the sent channel.
        interactionPermissions :: Maybe T.Text,
        -- | The invoking user's preferred locale.
        interactionLocale :: T.Text,
        -- | The invoking guild's preferred locale.
        interactionGuildLocale :: Maybe T.Text
      }
  deriving (Show, Read, Eq, Ord)

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
          permissions <- v .:? "app_permissions"
          t <- v .: "type" :: Parser Int
          case t of
            1 -> return $ InteractionPing iid aid tok version permissions
            2 ->
              InteractionApplicationCommand iid aid
                <$> v .: "data"
                <*> return gid
                <*> return cid
                <*> parseJSON (Object v)
                <*> return tok
                <*> return version
                <*> return permissions
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
                <*> return permissions
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
                <*> return permissions
                <*> v .: "locale"
                <*> return glocale
            5 ->
              InteractionModalSubmit iid aid
                <$> v .: "data"
                <*> return gid
                <*> return cid
                <*> parseJSON (Object v)
                <*> return tok
                <*> return version
                <*> return permissions
                <*> v .: "locale"
                <*> return glocale
            _ -> fail "unknown interaction type"
      )

newtype MemberOrUser = MemberOrUser (Either GuildMember User)
  deriving (Show, Read, Eq, Ord)

instance {-# OVERLAPPING #-} FromJSON MemberOrUser where
  parseJSON =
    withObject
      "MemberOrUser"
      ( \v -> MemberOrUser <$> (Left <$> v .: "member" <|> Right <$> v .: "user")
      )

data ComponentData
  = ButtonData
      { -- | The unique id of the component (up to 100 characters).
        componentDataCustomId :: T.Text
      }
  | SelectMenuData
      { -- | The unique id of the component (up to 100 characters).
        componentDataCustomId :: T.Text,
        -- | Values for the select menu.
        componentDataValues :: SelectMenuData
      }
  deriving (Show, Read, Eq, Ord)

instance FromJSON ComponentData where
  parseJSON =
    withObject
      "ComponentData"
      ( \v -> do
          cid <- v .: "custom_id"
          t <- v .: "component_type" :: Parser Int
          case t of
            2 -> return $ ButtonData cid
            _ | t `elem` [3, 5, 6, 7, 8] ->
              SelectMenuData cid
                <$> parseJSON (toJSON v)
            _ -> fail $ "unknown interaction data component type: " <> show t
      )

data SelectMenuData
  = SelectMenuDataText [T.Text] -- ^ The values of text chosen options
  | SelectMenuDataUser [UserId] -- ^ The users selected
  | SelectMenuDataRole [RoleId] -- ^ The roles selected
  | SelectMenuDataMentionable [Snowflake] -- ^ The users or roles selected
  | SelectMenuDataChannels [ChannelId] -- ^ The channels selected
  deriving (Show, Read, Eq, Ord)

instance FromJSON SelectMenuData where
  parseJSON =
    withObject
      "SelectMenuData"
      $ \v -> do
          t <- v .: "component_type" :: Parser Int
          let cons :: forall a. FromJSON a => ([a] -> SelectMenuData) -> Parser SelectMenuData
              cons f = f <$> v .: "values"
          case t of
            3 -> cons SelectMenuDataText
            5 -> cons SelectMenuDataUser
            6 -> cons SelectMenuDataRole
            7 -> cons SelectMenuDataMentionable
            8 -> cons SelectMenuDataChannels
            _ -> fail $ "unknown SelectMenuData type: " <> show t

data ApplicationCommandData
  = ApplicationCommandDataUser
      { -- | Id of the invoked command.
        applicationCommandDataId :: ApplicationCommandId,
        -- | Name of the invoked command.
        applicationCommandDataName :: T.Text,
        -- | The resolved data in the command.
        resolvedData :: Maybe ResolvedData,
        -- | The id of the user that is the target.
        applicationCommandDataTargetUserId :: UserId
      }
  | ApplicationCommandDataMessage
      { -- | Id of the invoked command.
        applicationCommandDataId :: ApplicationCommandId,
        -- | Name of the invoked command.
        applicationCommandDataName :: T.Text,
        -- | The resolved data in the command.
        resolvedData :: Maybe ResolvedData,
        -- | The id of the message that is the target.
        applicationCommandDataTargetMessageId :: MessageId
      }
  | ApplicationCommandDataChatInput
      { -- | Id of the invoked command.
        applicationCommandDataId :: ApplicationCommandId,
        -- | Name of the invoked command.
        applicationCommandDataName :: T.Text,
        -- | The resolved data in the command.
        resolvedData :: Maybe ResolvedData,
        -- | The options of the application command.
        optionsData :: Maybe OptionsData
      }
  deriving (Show, Read, Eq, Ord)

instance FromJSON ApplicationCommandData where
  parseJSON =
    withObject
      "ApplicationCommandData"
      ( \v -> do
          aci <- v .: "id"
          name <- v .: "name"
          rd <- v .:? "resolved_data"
          t <- v .: "type" :: Parser Int
          case t of
            1 ->
              ApplicationCommandDataChatInput aci name rd
                <$> v .:? "options"
            2 ->
              ApplicationCommandDataUser aci name rd
                <$> v .: "target_id"
            3 ->
              ApplicationCommandDataMessage aci name rd
                <$> v .: "target_id"
            _ -> fail "unknown interaction data component type"
      )

-- | Either subcommands and groups, or values.
data OptionsData
  = OptionsDataSubcommands [OptionDataSubcommandOrGroup]
  | OptionsDataValues [OptionDataValue]
  deriving (Show, Read, Eq, Ord)

instance FromJSON OptionsData where
  parseJSON =
    withArray
      "OptionsData"
      ( \a -> do
          let a' = toList a
          case a' of
            [] -> return $ OptionsDataValues []
            (v' : _) ->
              withObject
                "OptionsData item"
                ( \v -> do
                    t <- v .: "type" :: Parser Int
                    if t == 1 || t == 2
                      then OptionsDataSubcommands <$> mapM parseJSON a'
                      else OptionsDataValues <$> mapM parseJSON a'
                )
                v'
      )

-- | Either a subcommand group or a subcommand.
data OptionDataSubcommandOrGroup
  = OptionDataSubcommandGroup
      { optionDataSubcommandGroupName :: T.Text,
        optionDataSubcommandGroupOptions :: [OptionDataSubcommand],
        optionDataSubcommandGroupFocused :: Bool
      }
  | OptionDataSubcommandOrGroupSubcommand OptionDataSubcommand
  deriving (Show, Read, Eq, Ord)

instance FromJSON OptionDataSubcommandOrGroup where
  parseJSON =
    withObject
      "OptionDataSubcommandOrGroup"
      ( \v -> do
          t <- v .: "type" :: Parser Int
          case t of
            2 ->
              OptionDataSubcommandGroup
                <$> v .: "name"
                <*> v .: "options"
                <*> v .:? "focused" .!= False
            1 -> OptionDataSubcommandOrGroupSubcommand <$> parseJSON (Object v)
            _ -> fail "unexpected subcommand group type"
      )

-- | Data for a single subcommand.
data OptionDataSubcommand = OptionDataSubcommand
  { optionDataSubcommandName :: T.Text,
    optionDataSubcommandOptions :: [OptionDataValue],
    optionDataSubcommandFocused :: Bool
  }
  deriving (Show, Read, Eq, Ord)

instance FromJSON OptionDataSubcommand where
  parseJSON =
    withObject
      "OptionDataSubcommand"
      ( \v -> do
          t <- v .: "type" :: Parser Int
          case t of
            1 ->
              OptionDataSubcommand
                <$> v .: "name"
                <*> v .:? "options" .!= []
                <*> v .:? "focused" .!= False
            _ -> fail "unexpected subcommand type"
      )

-- | Data for a single value.
data OptionDataValue
  = OptionDataValueString
      { optionDataValueName :: T.Text,
        optionDataValueString :: Either T.Text T.Text
      }
  | OptionDataValueInteger
      { optionDataValueName :: T.Text,
        optionDataValueInteger :: Either T.Text Integer
      }
  | OptionDataValueBoolean
      { optionDataValueName :: T.Text,
        optionDataValueBoolean :: Bool
      }
  | OptionDataValueUser
      { optionDataValueName :: T.Text,
        optionDataValueUser :: UserId
      }
  | OptionDataValueChannel
      { optionDataValueName :: T.Text,
        optionDataValueChannel :: ChannelId
      }
  | OptionDataValueRole
      { optionDataValueName :: T.Text,
        optionDataValueRole :: RoleId
      }
  | OptionDataValueMentionable
      { optionDataValueName :: T.Text,
        optionDataValueMentionable :: Snowflake
      }
  | OptionDataValueNumber
      { optionDataValueName :: T.Text,
        optionDataValueNumber :: Either T.Text Number
      }
  deriving (Show, Read, Eq, Ord)

instance FromJSON OptionDataValue where
  parseJSON =
    withObject
      "OptionDataValue"
      ( \v -> do
          name <- v .: "name"
          focused <- v .:? "focused" .!= False
          t <- v .: "type" :: Parser Int
          case t of
            3 ->
              OptionDataValueString name
                <$> parseValue v focused
            4 ->
              OptionDataValueInteger name
                <$> parseValue v focused
            10 ->
              OptionDataValueNumber name
                <$> parseValue v focused
            5 ->
              OptionDataValueBoolean name
                <$> v .: "value"
            6 ->
              OptionDataValueUser name
                <$> v .: "value"
            7 ->
              OptionDataValueChannel name
                <$> v .: "value"
            8 ->
              OptionDataValueRole name
                <$> v .: "value"
            9 ->
              OptionDataValueMentionable name
                <$> v .: "value"
            _ -> fail $ "unexpected interaction data application command option value type: " ++ show t
      )

data ModalData = ModalData
  { -- | The unique id of the component (up to 100 characters).
    modalDataCustomId :: T.Text,
    -- | Components from the modal.
    modalDataComponents :: [TextInput]
  }
  deriving (Show, Read, Eq, Ord)

instance FromJSON ModalData where
  parseJSON =
    withObject
      "ModalData"
      ( \v ->
          ModalData <$> v .: "custom_id"
            <*> ((v .: "components") >>= (join <$>) . mapM getTextInput)
      )
    where
      getTextInput :: Value -> Parser [TextInput]
      getTextInput = withObject "ModalData.TextInput" $ \o -> do
        t <- o .: "type" :: Parser Int
        case t of
          1 -> o .: "components"
          _ -> fail $ "expected action row type (1), got: " ++ show t

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
    resolvedDataChannels :: Maybe Value,
    resolvedDataMessages :: Maybe Value,
    resolvedDataAttachments :: Maybe Value
  }
  deriving (Show, Read, Eq, Ord)

instance ToJSON ResolvedData where
  toJSON ResolvedData {..} =
    objectFromMaybes
      [ "users" .=? resolvedDataUsers,
        "members" .=? resolvedDataMembers,
        "roles" .=? resolvedDataRoles,
        "channels" .=? resolvedDataChannels,
        "messages" .=? resolvedDataMessages,
        "attachments" .=? resolvedDataAttachments
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
            <*> v .:? "messages"
            <*> v .:? "attachments"
      )

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
  | -- | respond with a popup modal
    InteractionResponseModal InteractionResponseModalData
  deriving (Show, Read, Eq, Ord)

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
  toJSON (InteractionResponseModal ms) = object [("type", Number 9), ("data", toJSON ms)]

data InteractionResponseAutocomplete
  = InteractionResponseAutocompleteString [Choice T.Text]
  | InteractionResponseAutocompleteInteger [Choice Integer]
  | InteractionResponseAutocompleteNumber [Choice Number]
  deriving (Show, Read, Eq, Ord)

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
    interactionResponseMessageComponents :: Maybe [ActionRow],
    interactionResponseMessageAttachments :: Maybe [Attachment]
  }
  deriving (Show, Read, Eq, Ord)

-- | A basic interaction response, sending back the given text. This is
-- effectively a helper function.
interactionResponseMessageBasic :: T.Text -> InteractionResponseMessage
interactionResponseMessageBasic t = InteractionResponseMessage Nothing (Just t) Nothing Nothing Nothing Nothing Nothing

instance ToJSON InteractionResponseMessage where
  toJSON InteractionResponseMessage {..} =
    objectFromMaybes
      [ "tts" .=? interactionResponseMessageTTS,
        "content" .=? interactionResponseMessageContent,
        "embeds" .=? ((createEmbed <$>) <$> interactionResponseMessageEmbeds),
        "allowed_mentions" .=? interactionResponseMessageAllowedMentions,
        "flags" .=? interactionResponseMessageFlags,
        "components" .=? interactionResponseMessageComponents,
        "attachments" .=? interactionResponseMessageAttachments
      ]

-- | Types of flags to attach to the interaction message.
--
-- Currently the only flag is EPHERMERAL, which means only the user can see the
-- message.
data InteractionResponseMessageFlag = InteractionResponseMessageFlagEphermeral
  deriving (Show, Read, Eq, Ord)

newtype InteractionResponseMessageFlags = InteractionResponseMessageFlags [InteractionResponseMessageFlag]
  deriving (Show, Read, Eq, Ord)

instance Enum InteractionResponseMessageFlag where
  fromEnum InteractionResponseMessageFlagEphermeral = 1 `shift` 6
  toEnum i
    | i == 1 `shift` 6 = InteractionResponseMessageFlagEphermeral
    | otherwise = error $ "could not find InteractionCallbackDataFlag `" ++ show i ++ "`"

instance ToJSON InteractionResponseMessageFlags where
  toJSON (InteractionResponseMessageFlags fs) = Number $ fromInteger $ fromIntegral $ foldr (.|.) 0 (fromEnum <$> fs)

data InteractionResponseModalData = InteractionResponseModalData
  { interactionResponseModalCustomId :: T.Text,
    interactionResponseModalTitle :: T.Text,
    interactionResponseModalComponents :: [TextInput]
  }
  deriving (Show, Read, Eq, Ord)

instance ToJSON InteractionResponseModalData where
  toJSON InteractionResponseModalData {..} =
    object
      [ ("custom_id", toJSON interactionResponseModalCustomId),
        ("title", toJSON interactionResponseModalTitle),
        ("components", toJSON $ map (\ti -> object [("type", Number 1), ("components", toJSON [ti])]) interactionResponseModalComponents)
      ]
