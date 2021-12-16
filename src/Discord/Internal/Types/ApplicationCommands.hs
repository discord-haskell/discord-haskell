{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}

{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Discord.Internal.Types.ApplicationCommands where
import Discord.Internal.Types.Prelude
import Data.Aeson
import Data.Data
import Data.Maybe (fromJust)
import Control.Applicative
import Discord.Internal.Types.User (User)
import Discord.Internal.Types.Guild (GuildMember)
import Data.Text (unpack, Text)
import Discord.Internal.Types.Channel (Message, AllowedMentions, Attachment)
import Discord.Internal.Types.Embed
import Data.Bits
-- import Discord.Internal.Rest.Channel (AllowedMentions)
-- import Discord.Internal.Types (User(User))

toMaybeJSON :: (ToJSON a) => a -> Maybe Value
toMaybeJSON = return . toJSON

makeTable :: (Data t, Enum t) => t -> [(Int,t)]
makeTable t = map (\cData -> let c = fromConstr cData in (fromEnum c, c) ) (dataTypeConstrs $ dataTypeOf t)

data ApplicationCommandType = ACTCHAT_INPUT | ACTUSER | ACTMESSAGE
    deriving (Show, Read, Data, Eq)
instance Enum ApplicationCommandType where
    fromEnum ACTCHAT_INPUT = 1
    fromEnum ACTUSER = 2
    fromEnum ACTMESSAGE = 3
    toEnum a = fromJust $ lookup a table
        where table = makeTable ACTCHAT_INPUT

instance ToJSON ApplicationCommandType where
    toJSON = toJSON . fromEnum

instance FromJSON ApplicationCommandType where
    parseJSON = withScientific "ApplicationCommandType" (return . toEnum . round)

--guild commands are approved instantly so that you can test quickly
--global commands may take an hour

-- makeSlashCommand :: String -> -> String ->Maybe Snowflake -> ApplicationCommand

type ApplicationCommandId = Snowflake

data CreateApplicationCommand = CreateApplicationCommand
    { cApplicationCommandName :: String
    , cApplicationCommandDescription :: String
    , cApplicationCommandOptions :: Maybe [ApplicationCommandOption]
    , cApplicationCommandDefaultPermission :: Maybe Bool
    , cApplicationCommandType :: Maybe ApplicationCommandType
}

instance ToJSON CreateApplicationCommand where
    toJSON CreateApplicationCommand{..} = object [(name, value) | (name, Just value) <-
      [ ("name", toMaybeJSON cApplicationCommandName)
      , ("description", toMaybeJSON cApplicationCommandDescription)
      , ("options", toJSON <$> cApplicationCommandOptions)
      , ("default_permission", toJSON <$> cApplicationCommandDefaultPermission)
      , ("type", toJSON <$> cApplicationCommandType)
      ] ]

-- instance FromJSON CreateApplicationCommand where
--     parseJSON = withObject "CreateApplicationCommand" (\v -> CreateApplicationCommand
--         <$> v .:  "name"
--         <*> v .:  "description"
--         <*> v .:? "options"
--         <*> v .:? "default_permission"
--         <*> v .:? "type")

-- convertCACToAC :: CreateApplicationCommand -> ApplicationCommand
-- convertCACToAC CreateApplicationCommand{..} = ApplicationCommand 0 cApplicationCommandType 0 Nothing cApplicationCommandName cApplicationCommandDescription cApplicationCommandOptions cApplicationCommandDefaultPermission 0

data EditApplicationCommand = EditApplicationCommand
    { eApplicationCommandName :: Maybe String
    , eApplicationCommandDescription :: Maybe String
    , eApplicationCommandOptions :: Maybe [ApplicationCommandOption]
    , eApplicationCommandDefaultPermission :: Maybe Bool
    , eApplicationCommandType :: Maybe ApplicationCommandType
}

instance ToJSON EditApplicationCommand where
    toJSON EditApplicationCommand{..} = object [(name, value) | (name, Just value) <-
      [ ("name", toJSON <$> eApplicationCommandName)
      , ("description", toJSON <$> eApplicationCommandDescription)
      , ("options", toJSON <$> eApplicationCommandOptions)
      , ("default_permission", toJSON <$> eApplicationCommandDefaultPermission)
      , ("type", toJSON <$> eApplicationCommandType)
      ] ]

-- instance FromJSON EditApplicationCommand where
--     parseJSON = withObject "ApplicationCommand" (\v -> EditApplicationCommand
--         <$> v .:? "name"
--         <*> v .:? "description"
--         <*> v .:? "options"
--         <*> v .:? "default_permission"
--         <*> v .:? "type")

-- https://discord.com/developers/docs/interactions/application-commands#application-command-object-application-command-structure
data ApplicationCommand = ApplicationCommand
    { applicationCommandId :: ApplicationCommandId -- ^ unique id of the command
    , applicationCommandType :: Maybe ApplicationCommandType -- ^ the type of the command, 1, 2 or 3
    , applicationCommandApplicationId        :: ApplicationId  -- ^ unique id of the parent application	
    , applicationCommandGuildId        :: Maybe Snowflake -- ^ the guild id of the command if not global
    , applicationCommandName :: String -- ^ 1-32 characters
    , applicationCommandDescription :: String -- ^ must be empty for USER and MESSAGE commands, 1-100 chars
    , applicationCommandOptions     :: Maybe [ApplicationCommandOption] -- ^ CHAT_INPUT only, parameters to command
    , applicationCommandDefaultPermission             :: Maybe Bool -- ^ whether the command is enabled by default when the app is added to a guild. Defaults to true.
    , applicationCommandVersion :: Snowflake
    }
    deriving (Show)

instance ToJSON ApplicationCommand where
    toJSON ApplicationCommand{..} = object [(name, value) | (name, Just value) <-
      [ ("id", toMaybeJSON applicationCommandId)
      , ("type", toJSON <$> applicationCommandType)
      , ("application_id", toMaybeJSON applicationCommandApplicationId)
      , ("guild_id", toJSON <$> applicationCommandGuildId)
      , ("name", toMaybeJSON applicationCommandName)
      , ("description", toMaybeJSON applicationCommandDescription)
      , ("options", toJSON <$> applicationCommandOptions)
      , ("default_permission", toJSON <$> applicationCommandDefaultPermission)
      , ("version", toMaybeJSON applicationCommandVersion)
      ] ]

instance FromJSON ApplicationCommand where
    parseJSON = withObject "ApplicationCommand" (\v -> ApplicationCommand
        <$> v .:  "id"
        <*> v .:? "type"
        <*> v .:  "application_id"
        <*> v .:? "guild_id"
        <*> v .:  "name"
        <*> v .:  "description"
        <*> v .:? "options"
        <*> v .:? "default_permission"
        <*> v .:  "version")



data ApplicationCommandOption = ApplicationCommandOption
    { optionType :: ApplicationCommandOptionType
    , optionName :: String -- ^ 1-32 characters
    , optionDescription :: String -- ^ 1-100 characters
    , optionRequired :: Maybe Bool -- ^ is the parameter required? default false
    , optionChoices :: Maybe [ApplicationCommandOptionChoice] -- ^ if specified, these are the only valid options to choose from. type depends on optionType
    , optionOptions :: Maybe [ApplicationCommandOption] -- ^ if the option type is a subcommand or subcommand group type, these are the parameters
    , optionChannelTypes :: Maybe [ApplicationCommandChannelType] -- ^ if option is channel type, these are the only types allowed
    , optionMinVal :: Maybe (Either Int Double) -- ^ if option is number type, minimum value for the number
    , optionMaxVal :: Maybe (Either Int Double) -- ^ if option is number type, maximum value for the number
    , optionAutocomplete :: Maybe Bool -- ^ enable auto complete interactions. may not be set to true if choices is present
    }
    deriving (Show)

instance ToJSON ApplicationCommandOption where
    toJSON ApplicationCommandOption{..} = object [(name, value) | (name, Just value) <-
      [ ("type", toMaybeJSON optionType)
      , ("name", toMaybeJSON optionName)
      , ("description", toMaybeJSON optionDescription)
      , ("required", toJSON <$> optionRequired)
      , ("choices", toJSON <$> optionChoices)
      , ("options", toJSON <$> optionOptions)
      , ("channel_types", toJSON <$> optionChannelTypes)
      , ("min_val", toJSON <$> optionMinVal)
      , ("max_val", toJSON <$> optionMaxVal)
      , ("autocomplete", toJSON <$> optionAutocomplete)
      ] ]

instance FromJSON ApplicationCommandOption where
    parseJSON =withObject "ApplicationCommandOption" (\v -> ApplicationCommandOption
        <$> v .:  "type"
        <*> v .:  "name"
        <*> v .:  "description"
        <*> v .:? "required"
        <*> v .:? "choices"
        <*> v .:? "options"
        <*> v .:? "channel_types"
        <*> v .:? "min_val"
        <*> v .:? "max_val"
        <*> v .:? "autocomplete")

data ApplicationCommandOptionType = SUB_COMMAND | SUB_COMMAND_GROUP | STRING | INTEGER | BOOLEAN | USER | CHANNEL | ROLE | MENTIONABLE | NUMBER
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
        where table = makeTable SUB_COMMAND

instance ToJSON ApplicationCommandOptionType where
    toJSON = toJSON . fromEnum

instance FromJSON ApplicationCommandOptionType where
    parseJSON = withScientific "ApplicationCommandOptionType" (return . toEnum . round)

data StringIntDouble = SIDS String | SIDI Int | SIDD Double
    deriving (Show, Read, Eq)

instance ToJSON StringIntDouble where
    toJSON (SIDS s) = toJSON s
    toJSON (SIDI i) = toJSON i
    toJSON (SIDD d) = toJSON d

instance FromJSON StringIntDouble where
    parseJSON (String t) = return $ SIDS $ unpack t
    parseJSON v = (SIDI <$> parseJSON v) <|> (SIDD <$> parseJSON v)


data ApplicationCommandOptionChoice = ApplicationCommandOptionChoice
    { choiceName :: String
    , choiceValue :: StringIntDouble
    }
    deriving (Show, Read, Eq)

instance ToJSON ApplicationCommandOptionChoice where
    toJSON ApplicationCommandOptionChoice{..} = object [("name",toJSON choiceName), ("value",toJSON choiceValue)]

instance FromJSON ApplicationCommandOptionChoice where
    parseJSON = withObject "ApplicationCommandOptionChoice" (\v-> ApplicationCommandOptionChoice
        <$> v .: "name"
        <*> v .: "value")

data ApplicationCommandChannelType = GUILD_TEXT | DM | GUILD_VOICE | GROUP_DM | GUILD_CATEGORY | GUILD_NEWS | GUILD_STORE | GUILD_NEWS_THREAD | GUILD_PUBLIC_THREAD | GUILD_PRIVATE_THREAD | GUILD_STAGE_VOICE
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
        where table = makeTable GUILD_TEXT

instance ToJSON ApplicationCommandChannelType where
    toJSON = toJSON . fromEnum

instance FromJSON ApplicationCommandChannelType where
    parseJSON = withScientific "ApplicationCommandChannelType" (return . toEnum . round)

type InteractionId = Snowflake
type InteractionToken = Text

data Interaction = Interaction
    { interactionId     :: InteractionId
    , interactionApplicationId     :: ApplicationId 
    , interactionType   :: InteractionType -- referenced as Type in API
    , interactionData   :: Maybe InteractionData       -- referenced as Data in API
    , interactionGuildId           :: Maybe GuildId
    , interactionChannelId         :: Maybe ChannelId
    , interactionMember            :: Maybe GuildMember
    , interactionUser              :: Maybe User
    , interactionToken             :: InteractionToken
    , interactionVersion           :: Int
    , interactionMessage :: Maybe Message
    } deriving (Show, Read, Eq)

instance ToJSON Interaction where
    toJSON Interaction{..} = object [(name, value) | (name, Just value) <-
      [ ("id", toMaybeJSON interactionId)
      , ("application_id", toMaybeJSON interactionApplicationId)
      , ("type", toMaybeJSON interactionType)
      , ("data", toJSON <$> interactionData)
      , ("guild_id", toJSON <$> interactionGuildId)
      , ("channel_id", toJSON <$> interactionChannelId)
      , ("member", toJSON <$> interactionMember)
      , ("user", toJSON <$> interactionUser)
      , ("token", toMaybeJSON interactionToken)
      , ("version", toMaybeJSON interactionVersion)
      , ("message", toJSON <$> interactionMessage)
      ] ]

instance FromJSON Interaction where
    parseJSON = withObject "Interaction" (\v -> Interaction
        <$> v .:  "id"
        <*> v .:  "application_id"
        <*> v .:  "type"
        <*> v .:? "data" 
        <*> v .:? "guild_id" 
        <*> v .:? "channel_id" 
        <*> v .:? "member" 
        <*> v .:? "user" 
        <*> v .:  "token"
        <*> v .:  "version"
        <*> v .:? "message")

data InteractionType = PING | APPLICATION_COMMAND | MESSAGE_COMPONENT | APPLICATION_COMMAND_AUTOCOMPLETE
    deriving (Show, Read, Data, Eq)

instance Enum InteractionType where
    fromEnum PING = 1
    fromEnum APPLICATION_COMMAND = 2
    fromEnum MESSAGE_COMPONENT = 3
    fromEnum APPLICATION_COMMAND_AUTOCOMPLETE = 4
    toEnum a = fromJust $ lookup a table
        where table = makeTable PING

instance ToJSON InteractionType where
    toJSON = toJSON . fromEnum

instance FromJSON InteractionType where
    parseJSON = withScientific "InteractionType" (return . toEnum . round)


data InteractionData = InteractionData
    { interactionDataApplicationCommandId         :: ApplicationCommandId
    , interactionDataApplicationCommandName       :: String
    , interactionDataApplicationCommandType              :: ApplicationCommandType
    , interactionDataResolved          :: Maybe ResolvedData
    , interactionDataOptions           :: Maybe [ApplicationCommandInteractionDataOption]
    -- , interactionDataCustomId :: Maybe String
    -- , interactionDataComponentType :: Maybe Int -- ^ this is likely to change in future if and when it's needed
    , interactionDataTargetId :: Maybe Snowflake  -- ^ this is the id of the user or message being targetteed by a user command or a message command
    } deriving (Show, Read, Eq)

instance ToJSON InteractionData where
    toJSON InteractionData{..} = object [(name, value) | (name, Just value) <-
      [ ("id", toMaybeJSON interactionDataApplicationCommandId)
      , ("name", toMaybeJSON interactionDataApplicationCommandName)
      , ("type", toMaybeJSON interactionDataApplicationCommandType)
      , ("resolved", toJSON <$> interactionDataResolved)
      , ("options", toJSON <$> interactionDataOptions)
    -- missing info relevant for components
    --   , ("custom_id", toJSON <$> interactionDataCustomId)
    --   , ("component_type", toJSON <$> interactionDataComponentType)
      , ("target_id", toJSON <$> interactionDataTargetId)
      ] ]

instance FromJSON InteractionData where
    parseJSON = withObject "InteractionData" (\v -> InteractionData
        <$> v .:  "id"
        <*> v .:  "name"
        <*> v .:  "type"
        <*> v .:? "resolved" 
        <*> v .:? "options" 
        -- <*> v .:? "custom_id" 
        -- <*> v .:? "component_type" 
        -- <*> v .:? "values"
        <*> v .:? "target_id")

-- | It's not worth the time working out how to create this stuff.
-- If you need to extract from these values, check out the link below.
-- https://discord.com/developers/docs/interactions/receiving-and-responding#interaction-object-resolved-data-structure
data ResolvedData = ResolvedData
   { resolvedDataUsers     :: Maybe Value
   , resolvedDataMembers   :: Maybe Value
   , resolvedDataRoles     :: Maybe Value
   , resolvedDataChannels  :: Maybe Value
   } deriving (Show, Read, Eq)

instance ToJSON ResolvedData where
    toJSON ResolvedData{..} = object [(name, value) | (name, Just value) <-
      [ ("users", resolvedDataUsers)
      , ("members",resolvedDataMembers)
      , ("roles",  resolvedDataRoles)
      , ("channels",  resolvedDataChannels)
      ] ]

instance FromJSON ResolvedData where
    parseJSON = withObject "ResolvedData" (\v -> ResolvedData
        <$> v .:?  "users"
        <*> v .:?  "members"
        <*> v .:? "roles" 
        <*> v .:? "channels")


data ApplicationCommandInteractionDataOption = ApplicationCommandInteractionDataOption
    { applicationCommandInteractionDataOptionName                 :: String
    , applicationCommandInteractionDataOptionType  :: ApplicationCommandOptionType
    , applicationCommandInteractionDataOptionValue                :: Maybe StringIntDouble
    , applicationCommandInteractionDataOptionOptions              :: Maybe[ApplicationCommandInteractionDataOption]
    , applicationCommandInteractionDataOptionFocused :: Maybe Bool 
    } deriving (Show, Read, Eq)

instance ToJSON ApplicationCommandInteractionDataOption where
    toJSON ApplicationCommandInteractionDataOption{..} = object [(name, value) | (name, Just value) <-
      [ ("name", toMaybeJSON applicationCommandInteractionDataOptionName)
      , ("type", toMaybeJSON applicationCommandInteractionDataOptionType)
      , ("value", toJSON <$> applicationCommandInteractionDataOptionValue)
      , ("options", toJSON <$> applicationCommandInteractionDataOptionOptions)
      , ("focused", toJSON <$> applicationCommandInteractionDataOptionFocused)
      ] ]

instance FromJSON ApplicationCommandInteractionDataOption where
    parseJSON = withObject "ApplicationCommandInteractionDataOption" (\v -> ApplicationCommandInteractionDataOption
        <$> v .:  "name"
        <*> v .:  "type"
        <*> v .:? "value" 
        <*> v .:? "options"
        <*> v .:? "focused")

data InteractionResponse = InteractionResponse 
    { interactionResponseType :: InteractionCallbackType
    , interactionResponseData :: Maybe InteractionCallbackData
    } deriving (Show, Read, Eq)

instance ToJSON InteractionResponse where
    toJSON InteractionResponse{..} = object [(name, value) | (name, Just value) <-
      [ ("type", toMaybeJSON interactionResponseType)
      , ("data", toJSON <$> interactionResponseData)
      ] ]

data InteractionCallbackType = PONG | CHANNEL_MESSAGE_WITH_SOURCE | DEFERRED_CHANNEL_MESSAGE_WITH_SOURCE | DEFERRED_UPDATE_MESSAGE | UPDATE_MESSAGE | APPLICATION_COMMAND_AUTOCOMPLETE_RESULT
    deriving (Show, Read, Eq, Data)

instance Enum InteractionCallbackType where
    fromEnum PONG = 1
    fromEnum CHANNEL_MESSAGE_WITH_SOURCE = 4
    fromEnum DEFERRED_CHANNEL_MESSAGE_WITH_SOURCE = 5
    fromEnum DEFERRED_UPDATE_MESSAGE = 6
    fromEnum UPDATE_MESSAGE = 6
    fromEnum APPLICATION_COMMAND_AUTOCOMPLETE_RESULT = 6
    toEnum a = fromJust $ lookup a table
        where table = makeTable PONG

instance ToJSON InteractionCallbackType where
    toJSON = toJSON . fromEnum

data InteractionCallbackData = ICDM InteractionCallbackDataMessages | ICDA InteractionCallbackDataAutocomplete
    deriving (Show, Read, Eq)

instance ToJSON InteractionCallbackData where
    toJSON (ICDM icdm) = toJSON icdm
    toJSON (ICDA icda) = toJSON icda

type InteractionCallbackDataAutocomplete = [ApplicationCommandOptionChoice]

data InteractionCallbackDataMessages = InteractionCallbackDataMessages 
    { interactionCallbackDataMessagesTTS :: Maybe Bool
    , interactionCallbackDataMessagesContent :: Maybe Text
    , interactionCallbackDataMessagesEmbeds :: Maybe [Embed]
    , interactionCallbackDataMessagesAllowedMentions :: Maybe [AllowedMentions]
    , interactionCallbackDataMessagesFlags :: Maybe InteractionCallbackDataFlags
    -- missing components
    , interactionCallbackDataMessagesAttachments :: Maybe [Attachment]
    } deriving (Show, Read, Eq)


instance ToJSON InteractionCallbackDataMessages where
    toJSON InteractionCallbackDataMessages{..} = object [(name, value) | (name, Just value) <-
      [ ("tts", toJSON <$> interactionCallbackDataMessagesTTS)
      , ("content", toJSON <$> interactionCallbackDataMessagesContent)
      , ("embeds", toJSON <$> interactionCallbackDataMessagesEmbeds)
      , ("allowed_mentions", toJSON <$> interactionCallbackDataMessagesAllowedMentions)
      , ("flags", toJSON <$> interactionCallbackDataMessagesFlags)
      , ("attachments", toJSON <$> interactionCallbackDataMessagesAttachments)
      ] ]


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

-- data ApplicationCommand = ApplicationCommand
--     { applicationCommandId :: Snowflake -- ^ unique id of the command
--     , applicationId        :: Snowflake 
--     , applicationCommandName :: String
--     , applicationCommandDescription :: String
--     , applicationCommandOptions     :: Maybe [ApplicationCommandOption]
--     , defaultPermission             :: Maybe Bool
--     }
-- instance ToJSON ApplicationCommand where
--     toJSON ApplicationCommand{..} = object [(name, value) | (name, Just value) <-
--       [ ("id", toJSON <$> pure applicationCommandId)
--       , ("application_id", toJSON <$> pure applicationId)
--       , ("name", toJSON <$> pure applicationCommandName)
--       , ("description", toJSON <$> pure applicationCommandDescription)
--       , ("options", toJSON <$> pure applicationCommandOptions)
--       , ("default_permission", toJSON <$> pure (case defaultPermission of {Nothing -> Just True; _ -> defaultPermission}))
--       ] ]


-- data ApplicationCommandOption = ApplicationCommandOption
--     { optionType :: Int
--     , optionName :: String
--     , optionDescription :: String
--     , optionRequired :: Maybe Bool
--     , optionChoices :: Maybe [ApplicationCommandOptionChoice]
--     , optionOptions :: Maybe [ApplicationCommandOption]
--     }

-- data ApplicationCommandPermissions = ApplicationCommandPermissions 
--     { applicationCommandPermissionId :: Snowflake
--     , commandPermissionType :: ApplicationCommandPermissionType
--     , commandPermission     :: Bool
--     }


-- type ApplicationCommandPermissionType = Int


-- data GuildApplicationCommandPermissions = GuildApplicationCommandPermissions 
--     { guildApplicationCommandPermissionId :: Snowflake
--     , guildApplicationCommandPermissionApplicationId :: Snowflake
--     , guildApplicationCommandPermissionGuildId      :: Snowflake
--     , guildApplicationCommandPermissionPermissions :: [ApplicationCommandPermissions]
--     }

-- instance ToJSON ApplicationCommandOption where
--     toJSON ApplicationCommandOption{..} = object [(name, value) | (name, Just value) <-
--         [ ("type", toJSON <$> pure optionType)
--         , ("name", toJSON <$> pure optionName)
--         , ("description", toJSON <$> pure optionDescription)
--         , ("required", toJSON <$> pure optionRequired)
--         , ("choices", toJSON <$> pure optionChoices)
--         , ("options", toJSON <$> pure optionOptions)
--         ] ]



-- instance ToJSON ApplicationCommandOptionChoice where
--     toJSON ApplicationCommandOptionChoice{..} = object [(name, value) | (name, Just value) <-
--         [ ("name", toJSON <$> pure choiceName)
--         , ("value", toJSON <$> Just (show choiceValue))
--         ] ]  

-- data Interaction = Interaction
--     { interactionId     :: Snowflake
--     , applicationId'     :: Snowflake
--     , interactionType   :: InteractionType -- referenced as Type in API
--     , interactionData   :: Maybe ApplicationCommandInteractionData       -- referenced as Data in API
--     , guildId           :: Maybe GuildId
--     , channelId         :: Maybe ChannelId
--     , member            :: GuildMember
--     , user              :: User
--     , token             :: String
--     , version           :: Int
--     } deriving (Show, Read, Eq)

-- -- instance ToJSON Interaction where
--     -- toJSON Interaction{..} = withObject "Interaction" $ \o ->
--     --     Interaction <$> o .: ""

-- -- | 1 corresponds to Ping
-- -- 
-- -- 2 corresponds to ApplicationCommand
-- type InteractionType = Int


-- data ApplicationCommandInteractionData = ApplicationCommandInteractionData
--     { commandId         :: Snowflake
--     , commandName       :: String
--     , resolved          :: Maybe ApplicationCommandInteractionDataResolved
--     , options           :: Maybe [ApplicationCommandInteractionDataOption]
--     } deriving (Show, Read, Eq)

-- data ApplicationCommandInteractionDataResolved = ApplicationCommandInteractionDataResolved
--    { resolvedUsers     :: Maybe Value
--    , resolvedMembers   :: Maybe Value
--    , resolvedRoles     :: Maybe Value
--    , resolvedChannels  :: Maybe Value
--    } deriving (Show, Read, Eq)


-- data ApplicationCommandInteractionDataOption = ApplicationCommandInteractionDataOption
--     { parameterName                 :: String
--     , applicationCommandOptionType  :: Int
--     , parameterValue                :: Maybe ApplicationCommandOptionType
--     , parameterOptions              :: [ApplicationCommandInteractionDataOption]
--     } deriving (Show, Read, Eq, Ord)


-- -- | NAME                           VALUE       DESCRIPTION
-- --  
-- -- Pong	                             1	        ACK a Ping
-- --  
-- -- ChannelMessageWithSource          4          Respond To an interaction with message
-- --
-- -- DeferredChannelMessageWithSource  5          ACK an interaction and edit a response later, the user sees a loading state
-- type InteractionCallbackType = Int



-- data InteractionApplicationCommandCallbackData = InteractionApplicationCommandCallbackData
--     { tts               :: Maybe Bool
--     , content           :: Maybe String
--     , embeds            :: Maybe [Embed]
--     , allowedMentions   :: Maybe AllowedMentions
--     , flags             :: Int
--     }
-- instance ToJSON InteractionApplicationCommandCallbackData where
--     toJSON InteractionApplicationCommandCallbackData{..} = object [(name, value) | (name, Just value) <-
--         [ ("tts", toJSON <$> pure tts)
--         , ("content", toJSON <$> pure content)
--         , ("embeds", toJSON <$> pure embeds)
--         , ("allowed_mentions", toJSON  <$> pure allowedMentions)
--         , ("flags", toJSON <$> pure flags)
--         ] ]



-- -- | ApplicationCommandOptionType references Values from https://discord.com/developers/docs/interactions/slash-commands#applicationcommandoptiontype
-- type ApplicationCommandOptionType = Int

-- data MessageInteraction = MessageInteraction
--     { messageInteractionId      :: Snowflake
--     , messageInteractionIdType  :: InteractionType
--     , messageInteractionIdname  :: String
--     , messageInteractionIduser  :: User
--     } deriving (Show, Read, Eq, Ord)