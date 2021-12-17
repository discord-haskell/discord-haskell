{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Discord.Internal.Rest.ApplicationCommands where

import Data.Aeson (ToJSON (toJSON), Value)
import Discord.Internal.Types.Prelude ( ApplicationId, GuildId )
import Discord.Internal.Rest.Prelude (JsonRequest (..), Request (..), RestIO, baseUrl, (//))
import Discord.Internal.Types.ApplicationCommands
import Network.HTTP.Req as R

instance Request (ApplicationCommandRequest a) where
  jsonRequest = applicationCommandJsonRequest
  majorRoute = applicationCommandMajorRoute

type AppCom = ApplicationCommand

type CreateAppCom = CreateApplicationCommand

data ApplicationCommandRequest a where
  GetGlobalApplicationCommands :: ApplicationId -> ApplicationCommandRequest [AppCom]
  CreateGlobalApplicationCommand :: ApplicationId -> CreateAppCom -> ApplicationCommandRequest AppCom
  GetGlobalApplicationCommand :: ApplicationId -> ApplicationCommandId -> ApplicationCommandRequest AppCom
  EditGlobalApplicationCommand :: ApplicationId -> ApplicationCommandId -> EditApplicationCommand -> ApplicationCommandRequest AppCom
  DeleteGlobalApplicationCommand :: ApplicationId -> ApplicationCommandId -> ApplicationCommandRequest ()
  BulkOverWriteGlobalApplicationCommand :: ApplicationId -> [CreateAppCom] -> ApplicationCommandRequest ()
  GetGuildApplicationCommands :: ApplicationId -> GuildId -> ApplicationCommandRequest [AppCom]
  CreateGuildApplicationCommand :: ApplicationId -> GuildId -> CreateAppCom -> ApplicationCommandRequest AppCom
  GetGuildApplicationCommand :: ApplicationId -> GuildId -> ApplicationCommandId -> ApplicationCommandRequest AppCom
  EditGuildApplicationCommand :: ApplicationId -> GuildId -> ApplicationCommandId -> CreateAppCom -> ApplicationCommandRequest AppCom
  DeleteGuildApplicationCommand :: ApplicationId -> GuildId -> ApplicationCommandId -> ApplicationCommandRequest ()
  BulkOverWriteGuildApplicationCommand :: ApplicationId -> GuildId -> [CreateAppCom] -> ApplicationCommandRequest ()

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
  where
    convert :: (ToJSON a) => a -> RestIO (ReqBodyJson Value)
    convert = (pure @RestIO) . R.ReqBodyJson . toJSON
