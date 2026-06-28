{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import Control.Exception
import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import qualified Data.ByteString as B
import qualified Data.Text.IO as T
import Text.Printf
import Discord
import Discord.Types
import Discord.Requests
import Discord.Interactions
import ExampleUtils

call :: Request (r a) => FromJSON a => r a -> DiscordHandler a
call = restCall >=> either (liftIO . throwIO) pure

onEvent :: GuildId -> Event -> DiscordHandler ()
onEvent guild event = do
  liftIO $ putStrLn $ take 120 $ show event
  case event of
    Ready _ _ _ _ _ _ (PartialApplication app _) -> onReady app guild
    InteractionCreate interaction -> onInteraction interaction
    _ -> pure ()

onReady :: ApplicationId -> GuildId -> DiscordHandler ()
onReady app guild = call $ BulkOverWriteGuildApplicationCommand app guild [testCreate]

onInteraction :: Interaction -> DiscordHandler ()
onInteraction interaction = case interaction of
    InteractionApplicationCommand {..} -> case applicationCommandData of
        ApplicationCommandDataChatInput {..} -> case applicationCommandDataName of
            "test" -> testHandle interactionApplicationId interactionId interactionToken
            command -> fail $ printf "invalid command %s" $ show command
        _ -> fail "invalid command data"
    _ -> fail "invalid interaction"

testCreate :: CreateApplicationCommand
testCreate = CreateApplicationCommandChatInput "test" Nothing "test attachment actions" Nothing Nothing Nothing Nothing

testHandle :: ApplicationId -> InteractionId -> InteractionToken -> DiscordHandler ()
testHandle app interaction token = go where
  go = do
    message <- create
    liftIO $ threadDelay 5000000
    edit message
  create = do
    photo <- liftIO $ B.readFile "examples/embed-photo.jpg"
    -- create a new response message with two uploads that get turned into attachments
    let uploads = [
          Upload "test.txt" Nothing Nothing "this is a test file",
          Upload "test.jpg" Nothing Nothing photo]
    let response = def {
      interactionResponseMessageContent = Just "test",
      interactionResponseMessageUploads = uploads }
    call $ CreateInteractionResponse interaction token $ InteractionResponseChannelMessage response
    call $ GetOriginalInteractionResponse app token
  edit message = do
    let [_textId, imageId] = attachmentId <$> messageAttachments message
    -- drop the text attachment and retain the image attachment
    let attachments = [RequestAttachment imageId Nothing Nothing Nothing]
    -- upload a new file that gets added to the retained attachments
    let uploads = [Upload "test-new.txt" Nothing Nothing "this is another test file"]
    let response = def {
      interactionResponseMessageContent = Just "edited",
      interactionResponseMessageAttachments = Just attachments,
      interactionResponseMessageUploads = uploads }
    void $ call $ EditOriginalInteractionResponse app token response

main :: IO ()
main = do
  token <- getToken
  guild <- getGuildId
  let intent = def { gatewayIntentMessageContent = False }
  let options = RunDiscordOpts token (pure ()) (pure ()) (onEvent guild) T.putStrLn False intent False
  result <- runDiscord options
  liftIO $ T.putStrLn result
