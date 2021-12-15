{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Discord.Internal.Types.SlashCommands where
import Discord.Internal.Types.Prelude
import Discord.Internal.Types.Guild
import Discord.Internal.Types.User
import Discord.Internal.Types.Embed
import Discord.Internal.Rest.Channel ( AllowedMentions )
import Data.Aeson


data ApplicationCommand = ApplicationCommand
    { applicationCommandId :: Snowflake
    , applicationId        :: Snowflake
    , applicationCommandName :: String
    , applicationCommandDescription :: String
    , applicationCommandOptions     :: Maybe [ApplicationCommandOption]
    , defaultPermission             :: Maybe Bool
    }
instance ToJSON ApplicationCommand where
    toJSON ApplicationCommand{..} = object [(name, value) | (name, Just value) <-
      [ ("id", toJSON <$> pure applicationCommandId)
      , ("application_id", toJSON <$> pure applicationId)
      , ("name", toJSON <$> pure applicationCommandName)
      , ("description", toJSON <$> pure applicationCommandDescription)
      , ("options", toJSON <$> pure applicationCommandOptions)
      , ("default_permission", toJSON <$> pure (case defaultPermission of {Nothing -> Just True; _ -> defaultPermission}))
      ] ]


data ApplicationCommandOption = ApplicationCommandOption
    { optionType :: Int
    , optionName :: String
    , optionDescription :: String
    , optionRequired :: Maybe Bool
    , optionChoices :: Maybe [ApplicationCommandOptionChoice]
    , optionOptions :: Maybe [ApplicationCommandOption]
    }

data ApplicationCommandPermissions = ApplicationCommandPermissions 
    { applicationCommandPermissionId :: Snowflake
    , commandPermissionType :: ApplicationCommandPermissionType
    , commandPermission     :: Bool
    }


type ApplicationCommandPermissionType = Int


data GuildApplicationCommandPermissions = GuildApplicationCommandPermissions 
    { guildApplicationCommandPermissionId :: Snowflake
    , guildApplicationCommandPermissionApplicationId :: Snowflake
    , guildApplicationCommandPermissionGuildId      :: Snowflake
    , guildApplicationCommandPermissionPermissions :: [ApplicationCommandPermissions]
    }

instance ToJSON ApplicationCommandOption where
    toJSON ApplicationCommandOption{..} = object [(name, value) | (name, Just value) <-
        [ ("type", toJSON <$> pure optionType)
        , ("name", toJSON <$> pure optionName)
        , ("description", toJSON <$> pure optionDescription)
        , ("required", toJSON <$> pure optionRequired)
        , ("choices", toJSON <$> pure optionChoices)
        , ("options", toJSON <$> pure optionOptions)
        ] ]


data ApplicationCommandOptionChoice = ApplicationCommandOptionChoice
    { choiceName :: String
    , choiceValue :: Either String Int 
    }

instance ToJSON ApplicationCommandOptionChoice where
    toJSON ApplicationCommandOptionChoice{..} = object [(name, value) | (name, Just value) <-
        [ ("name", toJSON <$> pure choiceName)
        , ("value", toJSON <$> Just (show choiceValue))
        ] ]  

data Interaction = Interaction
    { interactionId     :: Snowflake
    , applicationId'     :: Snowflake
    , interactionType   :: InteractionType -- referenced as Type in API
    , interactionData   :: Maybe ApplicationCommandInteractionData       -- referenced as Data in API
    , guildId           :: Maybe GuildId
    , channelId         :: Maybe ChannelId
    , member            :: GuildMember
    , user              :: User
    , token             :: String
    , version           :: Int
    } deriving (Show, Eq)

-- instance ToJSON Interaction where
    -- toJSON Interaction{..} = withObject "Interaction" $ \o ->
    --     Interaction <$> o .: ""

-- | 1 corresponds to Ping
-- 
-- 2 corresponds to ApplicationCommand
type InteractionType = Int


data ApplicationCommandInteractionData = ApplicationCommandInteractionData
    { commandId         :: Snowflake
    , commandName       :: String
    , resolved          :: Maybe ApplicationCommandInteractionDataResolved
    , options           :: Maybe [ApplicationCommandInteractionDataOption]
    } deriving (Show, Eq)

data ApplicationCommandInteractionDataResolved = ApplicationCommandInteractionDataResolved
   { resolvedUsers     :: Maybe Value
   , resolvedMembers   :: Maybe Value
   , resolvedRoles     :: Maybe Value
   , resolvedChannels  :: Maybe Value
   } deriving (Show, Eq)


data ApplicationCommandInteractionDataOption = ApplicationCommandInteractionDataOption
    { parameterName                 :: String
    , applicationCommandOptionType  :: Int
    , parameterValue                :: Maybe ApplicationCommandOptionType
    , parameterOptions              :: [ApplicationCommandInteractionDataOption]
    } deriving (Show, Eq, Ord)


-- | NAME                           VALUE       DESCRIPTION
--  
-- Pong	                             1	        ACK a Ping
--  
-- ChannelMessageWithSource          4          Respond To an interaction with message
--
-- DeferredChannelMessageWithSource  5          ACK an interaction and edit a response later, the user sees a loading state
type InteractionCallbackType = Int



data InteractionApplicationCommandCallbackData = InteractionApplicationCommandCallbackData
    { tts               :: Maybe Bool
    , content           :: Maybe String
    , embeds            :: Maybe [Embed]
    , allowedMentions   :: Maybe AllowedMentions
    , flags             :: Int
    }
instance ToJSON InteractionApplicationCommandCallbackData where
    toJSON InteractionApplicationCommandCallbackData{..} = object [(name, value) | (name, Just value) <-
        [ ("tts", toJSON <$> pure tts)
        , ("content", toJSON <$> pure content)
        , ("embeds", toJSON <$> pure embeds)
        , ("allowed_mentions", toJSON  <$> pure allowedMentions)
        , ("flags", toJSON <$> pure flags)
        ] ]



-- | ApplicationCommandOptionType references Values from https://discord.com/developers/docs/interactions/slash-commands#applicationcommandoptiontype
type ApplicationCommandOptionType = Int

data MessageInteraction = MessageInteraction
    { messageInteractionId      :: Snowflake
    , messageInteractionIdType  :: InteractionType
    , messageInteractionIdname  :: String
    , messageInteractionIduser  :: User
    } deriving (Show, Eq, Ord)