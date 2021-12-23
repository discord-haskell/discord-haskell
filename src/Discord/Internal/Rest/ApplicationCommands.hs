{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Discord.Internal.Rest.ApplicationCommands where

import Data.Aeson (ToJSON (toJSON), Value)
import Discord.Internal.Rest.Prelude (JsonRequest (..), Request (..), RestIO, baseUrl, (//))
import Discord.Internal.Types
import Network.HTTP.Req as R

instance Request (ApplicationCommandRequest a) where
  jsonRequest = applicationCommandJsonRequest
  majorRoute = applicationCommandMajorRoute

data ApplicationCommandRequest a where
  GetGlobalApplicationCommands :: ApplicationId -> ApplicationCommandRequest [InternalApplicationCommand]
  CreateGlobalApplicationCommand :: ApplicationId -> CreateApplicationCommand -> ApplicationCommandRequest InternalApplicationCommand
  GetGlobalApplicationCommand :: ApplicationId -> ApplicationCommandId -> ApplicationCommandRequest InternalApplicationCommand
  EditGlobalApplicationCommand :: ApplicationId -> ApplicationCommandId -> EditApplicationCommand -> ApplicationCommandRequest InternalApplicationCommand
  DeleteGlobalApplicationCommand :: ApplicationId -> ApplicationCommandId -> ApplicationCommandRequest ()
  BulkOverWriteGlobalApplicationCommand :: ApplicationId -> [CreateApplicationCommand] -> ApplicationCommandRequest ()
  GetGuildApplicationCommands :: ApplicationId -> GuildId -> ApplicationCommandRequest [InternalApplicationCommand]
  CreateGuildApplicationCommand :: ApplicationId -> GuildId -> CreateApplicationCommand -> ApplicationCommandRequest InternalApplicationCommand
  GetGuildApplicationCommand :: ApplicationId -> GuildId -> ApplicationCommandId -> ApplicationCommandRequest InternalApplicationCommand
  EditGuildApplicationCommand :: ApplicationId -> GuildId -> ApplicationCommandId -> CreateApplicationCommand -> ApplicationCommandRequest InternalApplicationCommand
  DeleteGuildApplicationCommand :: ApplicationId -> GuildId -> ApplicationCommandId -> ApplicationCommandRequest ()
  BulkOverWriteGuildApplicationCommand :: ApplicationId -> GuildId -> [CreateApplicationCommand] -> ApplicationCommandRequest ()
  GetGuildApplicationCommandPermissions :: ApplicationId -> GuildId -> ApplicationCommandRequest GuildApplicationCommandPermissions
  GetApplicationCommandPermissions :: ApplicationId -> GuildId -> ApplicationCommandId -> ApplicationCommandRequest GuildApplicationCommandPermissions
  EditApplicationCommandPermissions :: ApplicationId -> GuildId -> ApplicationCommandId -> [ApplicationCommandPermissions] -> ApplicationCommandRequest GuildApplicationCommandPermissions
  -- | The only parameters needed in the GuildApplicationCommandPermissions
  -- objects are id and permissions.
  BatchEditApplicationCommandPermissions :: ApplicationId -> GuildId -> [GuildApplicationCommandPermissions] -> ApplicationCommandRequest [GuildApplicationCommandPermissions]

-- TODO: permissions checks
-- GetGuildApplicationCommandPermissions :: ApplicationId -> GuildID -> ApplicationCommandR

applications :: ApplicationId -> R.Url 'R.Https
applications s = baseUrl /: "applications" // s

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
  (BatchEditApplicationCommandPermissions aid _ _) -> "appcom_perm " <> show aid

applicationCommandJsonRequest :: ApplicationCommandRequest a -> JsonRequest
applicationCommandJsonRequest a = case a of
  (GetGlobalApplicationCommands aid) ->
    Get (applications aid /: "commands") mempty
  (CreateGlobalApplicationCommand aid cac) ->
    Post (applications aid /: "commands") (convert cac) mempty
  (GetGlobalApplicationCommand aid aci) ->
    Get (applications aid /: "commands" // aci) mempty
  (EditGlobalApplicationCommand aid aci eac) ->
    Patch (applications aid /: "commands" // aci) (convert eac) mempty
  (DeleteGlobalApplicationCommand aid aci) ->
    Delete (applications aid /: "commands" // aci) mempty
  (BulkOverWriteGlobalApplicationCommand aid cacs) ->
    Put (applications aid /: "commands") (R.ReqBodyJson $ toJSON cacs) mempty
  (GetGuildApplicationCommands aid gid) ->
    Get (applications aid /: "guilds" // gid /: "commands") mempty
  (CreateGuildApplicationCommand aid gid cac) ->
    Post (applications aid /: "guilds" // gid /: "commands") (convert cac) mempty
  (GetGuildApplicationCommand aid gid aci) ->
    Get (applications aid /: "guilds" // gid /: "commands" // aci) mempty
  (EditGuildApplicationCommand aid gid aci eac) ->
    Patch (applications aid /: "guilds" // gid /: "commands" // aci) (convert eac) mempty
  (DeleteGuildApplicationCommand aid gid aci) ->
    Delete (applications aid /: "guilds" // gid /: "commands" // aci) mempty
  (BulkOverWriteGuildApplicationCommand aid gid cacs) ->
    Put (applications aid /: "guilds" // gid /: "commands") (R.ReqBodyJson $ toJSON cacs) mempty
  (GetGuildApplicationCommandPermissions aid gid) ->
    Get (applications aid /: "guilds" // gid /: "commands" /: "permissions") mempty
  (GetApplicationCommandPermissions aid gid cid) ->
    Get (applications aid /: "guilds" // gid /: "commands" // cid /: "permissions") mempty
  (EditApplicationCommandPermissions aid gid cid ps) ->
    Put (applications aid /: "guilds" // gid /: "commands" // cid /: "permissions") (R.ReqBodyJson $ toJSON (GuildApplicationCommandPermissions aid cid gid ps)) mempty
  (BatchEditApplicationCommandPermissions aid gid ps) ->
    Put (applications aid /: "guilds" // gid /: "commands" /: "permissions") (R.ReqBodyJson $ toJSON ps) mempty
  where
    convert :: (ToJSON a) => a -> RestIO (ReqBodyJson Value)
    convert = (pure @RestIO) . R.ReqBodyJson . toJSON
