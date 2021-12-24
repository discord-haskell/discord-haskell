-- allows "strings" to be Data.Text
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

import Control.Monad (forM_, void, when)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Debug.Trace
import Discord
import Discord.Internal.Rest.Prelude (baseUrl)
import qualified Discord.Requests as R
import Discord.Types
import UnliftIO (liftIO)
import UnliftIO.Concurrent

-- Allows this code to be an executable. See discord-haskell.cabal
main :: IO ()
main = pingpongExample

testserverid :: Snowflake
testserverid = -1

-- | Replies "pong" to every message that starts with "ping"
pingpongExample :: IO ()
pingpongExample = do
  tok <- TIO.readFile "./examples/auth-token.secret"

  -- open ghci and run  [[ :info RunDiscordOpts ]] to see available fields
  t <-
    runDiscord $
      def
        { discordToken = tok,
          discordOnStart = startHandler,
          discordOnEnd = liftIO $ putStrLn "Ended",
          discordOnEvent = eventHandler,
          discordOnLog = \s -> TIO.putStrLn s >> TIO.putStrLn ""
        }
  TIO.putStrLn t

-- If the start handler throws an exception, discord-haskell will gracefully shutdown
--     Use place to execute commands you know you want to complete
startHandler :: DiscordHandler ()
startHandler = do
  Right partialGuilds <- restCall R.GetCurrentUserGuilds

  let activity =
        Activity
          { activityName = "ping-pong",
            activityType = ActivityTypeGame,
            activityUrl = Nothing
          }
  let opts =
        UpdateStatusOpts
          { updateStatusOptsSince = Nothing,
            updateStatusOptsGame = Just activity,
            updateStatusOptsNewStatus = UpdateStatusOnline,
            updateStatusOptsAFK = False
          }
  sendCommand (UpdateStatus opts)

  chans' <- restCall $ R.GetGuildChannels testserverid
  either
    (const (return ()))
    ( \chans ->
        forM_
          (take 1 (filter isTextChannel chans))
          ( \channel ->
              restCall $
                R.CreateMessage
                  (channelId channel)
                  "Hello! I will reply to pings with pongs"
          )
    )
    chans'

-- | Example user command
exampleUserCommand :: Maybe CreateApplicationCommand
exampleUserCommand = createApplicationCommandUser "usercomm"

-- | Example slash command that has subcommands and multiple types of fields.
newExampleSlashCommand :: Maybe CreateApplicationCommand
newExampleSlashCommand =
  createApplicationCommandChatInput
    "subtest"
    "testing out subcommands"
    >>= \d ->
      Just $
        d
          { createApplicationCommandOptions =
              Just
                ( toInternal
                    <$> [ ApplicationCommandOptionSubcommandGroup
                            "frstsubcmdgrp"
                            "the sub command group"
                            [ ApplicationCommandOptionSubcommand
                                "frstsubcmd"
                                "the first sub command"
                                [ ApplicationCommandOptionValueString
                                    "onestringinput"
                                    "two options"
                                    (Just True)
                                    ( Just
                                        [ Choice "green" "green",
                                          Choice "red" "red"
                                        ]
                                    )
                                    Nothing,
                                  ApplicationCommandOptionValueInteger "oneintinput" "choices galore" Nothing Nothing Nothing Nothing Nothing
                                ]
                            ],
                          ApplicationCommandOptionSubcommandOrGroupSubcommand $
                            ApplicationCommandOptionSubcommand
                              "frstsubcmd"
                              "the first subcommand"
                              [ ApplicationCommandOptionValueString
                                  "onestringinput"
                                  "two options"
                                  (Just True)
                                  ( Just
                                      [ Choice "yellow" "yellow",
                                        Choice "blue" "blue"
                                      ]
                                  )
                                  Nothing
                              ],
                          ApplicationCommandOptionSubcommandOrGroupSubcommand $
                            ApplicationCommandOptionSubcommand
                              "sndsubcmd"
                              "the second subcommand"
                              [ ApplicationCommandOptionValueBoolean
                                  "trueorfalse"
                                  "true or false"
                                  (Just True),
                                ApplicationCommandOptionValueNumber
                                  "numbercomm"
                                  "number option"
                                  Nothing
                                  Nothing
                                  (Just 3.1415)
                                  (Just 101)
                                  (Just True)
                              ]
                        ]
                )
          }

-- | An example slash command.
exampleSlashCommand :: CreateApplicationCommand
exampleSlashCommand =
  CreateApplicationCommand
    "test"
    "here is a description"
    ( Just $
        toInternal
          <$> [ ApplicationCommandOptionValueString
                  "randominput"
                  "I shall not"
                  (Just True)
                  (Just [Choice "firstOpt" "yay", Choice "secondOpt" "nay"])
                  Nothing
              ]
    )
    Nothing
    Nothing

exampleInteractionResponse :: InteractionDataApplicationCommandOptions -> InteractionResponse
exampleInteractionResponse d@(InteractionDataApplicationCommandOptionsValues [InteractionDataApplicationCommandOptionValue {interactionDataApplicationCommandOptionValueValue = ApplicationCommandInteractionDataValueString s}]) =
  interactionResponseBasic (T.pack $ "Here's the reply! You chose: " ++ show s)
exampleInteractionResponse _ =
  interactionResponseBasic
    "Something unexpected happened - the value was not what I expected!"

-- If an event handler throws an exception, discord-haskell will continue to run
eventHandler :: Event -> DiscordHandler ()
eventHandler event = case event of
  MessageCreate m -> when (not (fromBot m) && isPing m) $ do
    void $ restCall (R.CreateReaction (messageChannelId m, messageId m) "eyes")
    threadDelay (2 * 10 ^ (6 :: Int))

    -- A very simple message.
    void $ restCall (R.CreateMessage (messageChannelId m) "Pong!")

    -- A more complex message. Text-to-speech, does not mention everyone nor
    -- the user, and uses Discord native replies.
    -- Use ":info" in ghci to explore the type
    let opts :: R.MessageDetailedOpts
        opts =
          def
            { R.messageDetailedContent = "Here's a more complex message, but doesn't ping @everyone!",
              R.messageDetailedTTS = True,
              R.messageDetailedAllowedMentions =
                Just $
                  def
                    { R.mentionEveryone = False,
                      R.mentionRepliedUser = False
                    },
              R.messageDetailedReference =
                Just $
                  def {referenceMessageId = Just $ messageId m}
            }
    void $ restCall (R.CreateMessageDetailed (messageChannelId m) opts)
    let opts' :: R.MessageDetailedOpts
        opts' =
          def
            { R.messageDetailedContent = "An example of a message with buttons!",
              R.messageDetailedComponents =
                Just
                  [ toInternal
                      ( ComponentActionRowButton
                          [ ComponentButton "Button 1" False ButtonStylePrimary "Button 1" (Just (Emoji (Just 0) "🔥" Nothing Nothing Nothing (Just False))),
                            ComponentButton "Button 2" True ButtonStyleSuccess "Button 2" Nothing,
                            ComponentButtonUrl baseUrl False "Button 3" Nothing
                          ]
                      ),
                    toInternal
                      ( ComponentActionRowSelectMenu
                          ( ComponentSelectMenu
                              "action select menu"
                              False
                              [ SelectOption "First option" "opt1" (Just "the only desc") Nothing Nothing,
                                SelectOption "Second option" "opt2" Nothing (Just (Emoji (Just 0) "😭" Nothing Nothing Nothing (Just False))) (Just True),
                                SelectOption "third option" "opt3" Nothing Nothing Nothing,
                                SelectOption "fourth option" "opt4" Nothing Nothing Nothing,
                                SelectOption "fifth option" "opt5" Nothing Nothing Nothing
                              ]
                              (Just "this is a place holder")
                              (Just 2)
                              (Just 5)
                          )
                      )
                  ]
            }
    void $ restCall (R.CreateMessageDetailed (messageChannelId m) opts')
  Ready _ _ _ _ _ _ pa@(PartialApplication i _) ->
    mapM_ (maybe (return ()) (void . restCall . R.CreateGuildApplicationCommand i testserverid)) [Just exampleSlashCommand, exampleUserCommand, newExampleSlashCommand]
  InteractionCreate InteractionApplicationCommand {interactionDataApplicationCommand = Just InteractionDataApplicationCommandUser {interactionDataApplicationCommandName = nm, interactionDataApplicationCommandTargetId = uid, ..}, ..} ->
    void $
      restCall
        (R.CreateInteractionResponse interactionId interactionToken (interactionResponseBasic $ "Command " <> nm <> T.pack (" selected user: " ++ show uid)))
  InteractionCreate InteractionApplicationCommand {interactionDataApplicationCommand = Just InteractionDataApplicationCommandChatInput {interactionDataApplicationCommandName = "test", interactionDataApplicationCommandOptions = Just d, ..}, ..} ->
    void $
      restCall
        (R.CreateInteractionResponse interactionId interactionToken (exampleInteractionResponse d))
  InteractionCreate InteractionComponent {interactionDataComponent = Just InteractionDataComponentButton {..}, ..} ->
    void $
      restCall
        ( R.CreateInteractionResponse interactionId interactionToken $
            InteractionResponse
              InteractionCallbackTypeChannelMessageWithSource
              ( Just . InteractionCallbackDataMessages $
                  ( interactionCallbackMessagesBasic $ "You pressed the button " <> interactionDataComponentCustomId
                  )
                    { interactionCallbackMessagesFlags = Just (InteractionCallbackDataFlags [InteractionCallbackDataFlagEphermeral])
                    }
              )
        )
  _ -> return ()
  -- e -> trace ("uncaught:" ++ show e) $ return ()

isTextChannel :: Channel -> Bool
isTextChannel (ChannelText {}) = True
isTextChannel _ = False

fromBot :: Message -> Bool
fromBot = userIsBot . messageAuthor

isPing :: Message -> Bool
isPing = ("ping" `T.isPrefixOf`) . T.toLower . messageContent
