{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

import Discord
import Discord.Types
import Discord.Interactions
import UnliftIO (liftIO)
import Data.List (find)
import Control.Monad (forM_)
import ExampleUtils (getToken, getGuildId)
import Data.Text (Text)
import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO)
import qualified Discord.Requests as R
import qualified Data.Text as T
import qualified Data.Text.IO as TIO


-- MAIN

main :: IO ()
main = do
  tok <- getToken
  testGuildId <- getGuildId

  botTerminationError <- runDiscord $ def
    { discordToken = tok
    , discordOnEvent = onDiscordEvent testGuildId
    -- If you are using application commands, you might not need
    -- message contents at all
    , discordGatewayIntent = def { gatewayIntentMessageContent = False }
    }

  echo $ "A fatal error occurred: " <> botTerminationError


-- UTILS

echo :: MonadIO m => Text -> m ()
echo = liftIO . TIO.putStrLn

showT :: Show a => a -> Text
showT = T.pack . show


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

onDiscordEvent :: GuildId -> Event -> DiscordHandler ()
onDiscordEvent testGuildId = \case
  Ready _ _ _ _ _ _ (PartialApplication appId _) -> onReady appId testGuildId
  InteractionCreate intr                         -> onInteractionCreate intr
  _                                              -> pure ()

onReady :: ApplicationId -> GuildId -> DiscordHandler ()
onReady appId testGuildId = do
  echo "Bot ready!"

  appCmdRegistrations <- mapM tryRegistering mySlashCommands

  case sequence appCmdRegistrations of
    Left err ->
      echo $ "[!] Failed to register some commands" <> showT err

    Right cmds -> do
      echo $ "Registered " <> showT (length cmds) <> " command(s)."
      unregisterOutdatedCmds cmds

  where
  tryRegistering cmd = case registration cmd of
    Just reg -> restCall $ R.CreateGuildApplicationCommand appId testGuildId reg
    Nothing  -> pure . Left $ RestCallErrorCode 0 "" ""

  unregisterOutdatedCmds validCmds = do
    registered <- restCall $ R.GetGuildApplicationCommands appId testGuildId
    case registered of
      Left err ->
        echo $ "Failed to get registered slash commands: " <> showT err

      Right cmds ->
        let validIds    = map applicationCommandId validCmds
            outdatedIds = filter (`notElem` validIds)
                        . map applicationCommandId
                        $ cmds
         in forM_ outdatedIds $
              restCall . R.DeleteGuildApplicationCommand appId testGuildId

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
