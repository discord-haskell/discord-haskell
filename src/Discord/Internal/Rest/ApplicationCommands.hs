{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Discord.Internal.Rest.ApplicationCommands where

import Data.Aeson (Value)
import Discord.Internal.Rest.Prelude
import Discord.Internal.Types
import Discord.Internal.Types.ApplicationCommands
    ( ApplicationCommandPermissions,
      GuildApplicationCommandPermissions(GuildApplicationCommandPermissions),
      EditApplicationCommand,
      CreateApplicationCommand,
      ApplicationCommand )
import Network.HTTP.Req as R

instance Request (ApplicationCommandRequest a) where
  jsonRequest = applicationCommandJsonRequest
  majorRoute = applicationCommandMajorRoute

-- | Requests related to application commands
data ApplicationCommandRequest a where
  -- | Fetch all of the global commands for your application. Returns an list of 'ApplicationCommand's.
  GetGlobalApplicationCommands :: ApplicationId
                               -> ApplicationCommandRequest [ApplicationCommand]
  -- | Create a new global command. Returns an 'ApplicationCommand'.
  --
  -- __Note__: Creating a command with the same name as an existing command for your application will overwrite the old command.
  CreateGlobalApplicationCommand :: ApplicationId
                                 -> CreateApplicationCommand
                                 -> ApplicationCommandRequest ApplicationCommand
  -- | Fetch a global command for your application. Returns an 'ApplicationCommand'.
  GetGlobalApplicationCommand :: ApplicationId
                              -> ApplicationCommandId
                              -> ApplicationCommandRequest ApplicationCommand
  -- | Edit a global command. Returns an 'ApplicationCommand'.
  --
  -- All fields are optional, but any fields provided will entirely overwrite the existing values of those fields.
  EditGlobalApplicationCommand :: ApplicationId
                               -> ApplicationCommandId
                               -> EditApplicationCommand
                               -> ApplicationCommandRequest ApplicationCommand
  -- | Delete a global command.
  DeleteGlobalApplicationCommand :: ApplicationId
                                 -> ApplicationCommandId
                                 -> ApplicationCommandRequest ()
  -- | Takes a list of 'CreateApplicationCommand', overwriting the existing global command list for this application.
  --
  -- __Note__: This will overwrite __all__ types of application commands: slash commands, user commands, and message commands.
  BulkOverWriteGlobalApplicationCommand :: ApplicationId
                                        -> [CreateApplicationCommand]
                                        -> ApplicationCommandRequest ()
  -- | Fetch all of the guild commands for your application for a specific guild. Returns an list of 'ApplicationCommands'.
  GetGuildApplicationCommands :: ApplicationId
                              -> GuildId
                              -> ApplicationCommandRequest [ApplicationCommand]
  -- | Create a new guild command. New guild commands will be available in the guild immediately.
  -- Returns an 'ApplicationCommand'.
  -- If the command did not already exist, it will count toward daily application command create limits.
  --
  -- __Note__: Creating a command with the same name as an existing command for your application will overwrite the old command.
  CreateGuildApplicationCommand :: ApplicationId
                                -> GuildId
                                -> CreateApplicationCommand
                                -> ApplicationCommandRequest ApplicationCommand
  -- | Fetch a guild command for your application. Returns an 'ApplicationCommand'
  GetGuildApplicationCommand :: ApplicationId
                             -> GuildId
                             -> ApplicationCommandId
                             -> ApplicationCommandRequest ApplicationCommand
  -- | Edit a guild command. Updates for guild commands will be available immediately. Returns an 'ApplicationCommand'.
  -- All fields are optional, but any fields provided will entirely overwrite the existing values of those fields.
  EditGuildApplicationCommand :: ApplicationId
                              -> GuildId
                              -> ApplicationCommandId
                              -> CreateApplicationCommand
                              -> ApplicationCommandRequest ApplicationCommand
  -- | Delete a guild command.
  DeleteGuildApplicationCommand :: ApplicationId
                                -> GuildId
                                -> ApplicationCommandId
                                -> ApplicationCommandRequest ()
  -- | Takes a list of `CreateApplicationCommand`, overwriting the existing command list for this application for the targeted guild.
  --
  -- __Note__: This will overwrite __all__ types of application commands: slash commands, user commands, and message commands.
  BulkOverWriteGuildApplicationCommand :: ApplicationId
                                       -> GuildId
                                       -> [CreateApplicationCommand]
                                       -> ApplicationCommandRequest ()
  -- | Fetches permissions for all commands for your application in a guild. 
  GetGuildApplicationCommandPermissions :: ApplicationId
                                        -> GuildId
                                        -> ApplicationCommandRequest GuildApplicationCommandPermissions
  -- | Fetches permissions for a specific command for your application in a guild.
  GetApplicationCommandPermissions :: ApplicationId
                                   -> GuildId
                                   -> ApplicationCommandId
                                   -> ApplicationCommandRequest GuildApplicationCommandPermissions
  -- | Edits command permissions for a specific command for your application.
  -- You can add up to 100 permission overwrites for a command.
  -- __Notes__:
  --
  --   * This endpoint will overwrite existing permissions for the command in that guild
  --   * This endpoint requires authentication with a Bearer token that has permission to manage the guild and its roles.
  --   * Deleting or renaming a command will permanently delete all permissions for the command
  EditApplicationCommandPermissions :: ApplicationId
                                    -> GuildId
                                    -> ApplicationCommandId
                                    -> [ApplicationCommandPermissions]
                                    -> ApplicationCommandRequest GuildApplicationCommandPermissions

-- | The base url for application commands
applications :: ApplicationId -> R.Url 'R.Https
applications s = baseUrl /: "applications" /~ s

-- | The major routes identifiers for `ApplicationCommandRequest`s
applicationCommandMajorRoute :: ApplicationCommandRequest a -> String
applicationCommandMajorRoute a = case a of
  (GetGlobalApplicationCommands aid) -> "get_glob_appcomm" <> show aid
  (CreateGlobalApplicationCommand aid _) -> "write_glob_appcomm" <> show aid
  (GetGlobalApplicationCommand aid _) -> "get_glob_appcomm" <> show aid
  (EditGlobalApplicationCommand aid _ _) -> "write_glob_appcomm" <> show aid
  (DeleteGlobalApplicationCommand aid _) -> "write_glob_appcomm" <> show aid
  (BulkOverWriteGlobalApplicationCommand aid _) -> "write_glob_appcomm" <> show aid
  (GetGuildApplicationCommands aid _) -> "get_appcomm" <> show aid
  (CreateGuildApplicationCommand aid _ _) -> "write_appcomm" <> show aid
  (GetGuildApplicationCommand aid _ _) -> "get_appcomm" <> show aid
  (EditGuildApplicationCommand aid _ _ _) -> "write_appcomm" <> show aid
  (DeleteGuildApplicationCommand aid _ _) -> "write_appcomm" <> show aid
  (BulkOverWriteGuildApplicationCommand aid _ _) -> "write_appcomm" <> show aid
  (GetGuildApplicationCommandPermissions aid _) -> "appcom_perm " <> show aid
  (GetApplicationCommandPermissions aid _ _) -> "appcom_perm " <> show aid
  (EditApplicationCommandPermissions aid _ _ _) -> "appcom_perm " <> show aid

-- | The `JsonRequest`s for `ApplicationCommandRequest`s
applicationCommandJsonRequest :: ApplicationCommandRequest a -> JsonRequest
applicationCommandJsonRequest a = case a of
  (GetGlobalApplicationCommands aid) ->
    Get (applications aid /: "commands") mempty
  (CreateGlobalApplicationCommand aid cac) ->
    Post (applications aid /: "commands") (convert cac) mempty
  (GetGlobalApplicationCommand aid aci) ->
    Get (applications aid /: "commands" /~ aci) mempty
  (EditGlobalApplicationCommand aid aci eac) ->
    Patch (applications aid /: "commands" /~ aci) (convert eac) mempty
  (DeleteGlobalApplicationCommand aid aci) ->
    Delete (applications aid /: "commands" /~ aci) mempty
  (BulkOverWriteGlobalApplicationCommand aid cacs) ->
    Put (applications aid /: "commands") (R.ReqBodyJson $ toJSON cacs) mempty
  (GetGuildApplicationCommands aid gid) ->
    Get (applications aid /: "guilds" /~ gid /: "commands") mempty
  (CreateGuildApplicationCommand aid gid cac) ->
    Post (applications aid /: "guilds" /~ gid /: "commands") (convert cac) mempty
  (GetGuildApplicationCommand aid gid aci) ->
    Get (applications aid /: "guilds" /~ gid /: "commands" /~ aci) mempty
  (EditGuildApplicationCommand aid gid aci eac) ->
    Patch (applications aid /: "guilds" /~ gid /: "commands" /~ aci) (convert eac) mempty
  (DeleteGuildApplicationCommand aid gid aci) ->
    Delete (applications aid /: "guilds" /~ gid /: "commands" /~ aci) mempty
  (BulkOverWriteGuildApplicationCommand aid gid cacs) ->
    Put (applications aid /: "guilds" /~ gid /: "commands") (R.ReqBodyJson $ toJSON cacs) mempty
  (GetGuildApplicationCommandPermissions aid gid) ->
    Get (applications aid /: "guilds" /~ gid /: "commands" /: "permissions") mempty
  (GetApplicationCommandPermissions aid gid cid) ->
    Get (applications aid /: "guilds" /~ gid /: "commands" /~ cid /: "permissions") mempty
  (EditApplicationCommandPermissions aid gid cid ps) ->
    Put (applications aid /: "guilds" /~ gid /: "commands" /~ cid /: "permissions") (R.ReqBodyJson $ toJSON (GuildApplicationCommandPermissions cid aid gid ps)) mempty
  where
    convert :: (ToJSON a) => a -> RestIO (ReqBodyJson Value)
    convert = (pure @RestIO) . R.ReqBodyJson . toJSON
