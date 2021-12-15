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

toMaybeJSON :: (ToJSON a) => a -> Maybe Value
toMaybeJSON = return . toJSON

makeTable :: (Data t, Enum t) => t -> [(Int,t)]
makeTable t = map (\cData -> let c = fromConstr cData in (fromEnum c, c) ) (dataTypeConstrs $ dataTypeOf t)

data ApplicationCommandType = ACTCHAT_INPUT | ACTUSER | ACTMESSAGE
    deriving (Show, Data)
instance Enum ApplicationCommandType where
    fromEnum ACTCHAT_INPUT = 1
    fromEnum ACTUSER = 2
    fromEnum ACTMESSAGE = 3
    toEnum a = fromJust $ lookup a table
        where table = makeTable ACTCHAT_INPUT

instance ToJSON ApplicationCommandType where
    toJSON = toJSON . fromEnum

--guild commands are approved instantly so that you can test quickly
--global commands may take an hour

-- makeSlashCommand :: String -> -> String ->Maybe Snowflake -> ApplicationCommand

-- https://discord.com/developers/docs/interactions/application-commands#application-command-object-application-command-structure
data ApplicationCommand = ApplicationCommand
    { applicationCommandId :: Snowflake -- ^ unique id of the command
    , applicationCommandType :: Maybe ApplicationCommandType -- ^ the type of the command, 1, 2 or 3
    , applicationCommandApplicationId        :: Snowflake -- ^ unique id of the parent application	
    , applicationCommandGuildId        :: Maybe Snowflake -- ^ the guild id of the command if not global
    , applicationCommandName :: String -- ^ 1-32 characters
    , applicationCommandDescription :: String -- ^ must be empty for USER and MESSAGE commands
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
      ] ]


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

data ApplicationCommandOptionType = SUB_COMMAND | SUB_COMMAND_GROUP | STRING | INTEGER | BOOLEAN | USER | CHANNEL | ROLE | MENTIONABLE | NUMBER
    deriving (Show, Data)

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


data ApplicationCommandOptionChoice = ApplicationCommandOptionChoice
    { choiceName :: String
    , choiceValue :: Either String (Either Int Double)
    }
    deriving (Show)

instance ToJSON ApplicationCommandOptionChoice where
    toJSON ApplicationCommandOptionChoice{..} = object [("name",toJSON choiceName), ("value",cv)]
        where cv = case choiceValue of
                (Left s) -> toJSON s
                (Right (Left i)) -> toJSON i
                (Right (Right d)) -> toJSON d

data ApplicationCommandChannelType = GUILD_TEXT | DM | GUILD_VOICE | GROUP_DM | GUILD_CATEGORY | GUILD_NEWS | GUILD_STORE | GUILD_NEWS_THREAD | GUILD_PUBLIC_THREAD | GUILD_PRIVATE_THREAD | GUILD_STAGE_VOICE
    deriving (Show, Data)


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
--     } deriving (Show, Eq)

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
--     } deriving (Show, Eq)

-- data ApplicationCommandInteractionDataResolved = ApplicationCommandInteractionDataResolved
--    { resolvedUsers     :: Maybe Value
--    , resolvedMembers   :: Maybe Value
--    , resolvedRoles     :: Maybe Value
--    , resolvedChannels  :: Maybe Value
--    } deriving (Show, Eq)


-- data ApplicationCommandInteractionDataOption = ApplicationCommandInteractionDataOption
--     { parameterName                 :: String
--     , applicationCommandOptionType  :: Int
--     , parameterValue                :: Maybe ApplicationCommandOptionType
--     , parameterOptions              :: [ApplicationCommandInteractionDataOption]
--     } deriving (Show, Eq, Ord)


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
--     } deriving (Show, Eq, Ord)