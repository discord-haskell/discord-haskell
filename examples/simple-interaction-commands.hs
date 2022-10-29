{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

import Discord
import Discord.Types
import Discord.Interactions
import UnliftIO (liftIO)
import Data.List (find)
import ExampleUtils (getToken)
import qualified Discord.Requests as R
import qualified Data.Text.IO as TIO


-- MAIN

main :: Text
main = do
  tok <- getToken

  botTerminationError <- runDiscord $ def
    { discordToken = tok
    , discordOnEvent = onDiscordEvent
    -- If you are using application commands, you might not need
    -- message contents at all
    , discordGatewayIntent = def { gatewayIntentMessageContent = False }
    }

  echo $ "A fatal error occurred: " <> botTerminationError


-- UTILS

echo :: MonadIO m => Text -> m ()
echo = liftIO . TIO.putStrLn


-- COMMANDS

data SlashCommand = SlashCommand
  { name :: Text
  , registration :: Maybe CreateApplicationCommand
  , handler :: Interaction -> Maybe OptionsData -> DiscordHandler ()
  }

mySlashCommands :: [SlashCommand]
mySlashCommands = [ping]

ping :: SlashCommand
ping = SlashCommand
  { name = "ping"
  , registration = createChatInput "ping" "responds pong"
  , handler = \intr _options ->
      void . restCall $
        R.CreateInteractionResponse
          (interactionId intr)
          (interactionToken intr)
          (interactionResponseBasic  "pong")
  }


-- EVENTS

onDiscordEvent :: Event -> DiscordHandler ()
onDiscordEvent = \case
  Ready _ _ _ _ _ _ (PartialApplication appId _) -> onReady appId
  InteractionCreate intr                         -> onInteractionCreate intr
  _                                              -> pure ()

onReady :: ApplicationId -> DiscordHandler ()
onReady appId = do
  echo "Bot ready!"

  appCmdRegistrations <- mapM tryRegistering mySlashCommands

  case sequence appCmdRegistrations of
    Left err ->
      echo $ "[!] Failed to register some commands" <> show err

    Right cmds -> do
      echo $ "Registered " <> show (length cmds) <> " command(s)."
      unregisterOutdatedCmds cmds

  where
  tryRegistering cmd = case registration cmd of
    Just reg -> restCall    $ R.CreateGlobalApplicationCommand appId reg
    Nothing  -> pure . Left $ RestCallErrorCode 0 "" ""

  unregisterOutdatedCmds validCmds = do
    registered <- restCall $ R.GetGlobalApplicationCommands appId
    case registered of
      Left err -> echo $ "Failed to get registered slash commands: " <> show err
      Right cmds -> do
        let validIds    = map applicationCommandId validCmds
            outdatedIds = filter (`notElem` validIds)
                        . map applicationCommandId
                        $ cmds
        mapM_ (restCall . R.DeleteGlobalApplicationCommand appId) outdatedIds

onInteractionCreate :: Interaction -> DiscordHandler ()
onInteractionCreate = \case
  cmd@InteractionApplicationCommand
    { applicationCommandData = input@ApplicationCommandDataChatInput {} } ->
      case
        find (\c -> applicationCommandDataName input == name c) mySlashCommands
      of
        Just found ->
          handler found cmd (optionsData input)

        Nothing ->
          echo "Somehow got unknown slash command (registrations out of date?)"

  _ ->
    pure () -- Unexpected/unsupported interaction type
