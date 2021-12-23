-- allows "strings" to be Data.Text
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

import Control.Monad (forM_, void, when)
import Data.Char (toLower)
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
exampleUserCommand :: CreateApplicationCommand
exampleUserCommand =
  def
    { createApplicationCommandName = "usercomm",
      createApplicationCommandType = Just ApplicationCommandTypeUser
    }

-- | Example slash command that has subcommands and multiple types of fields.
newExampleSlashCommand :: CreateApplicationCommand
newExampleSlashCommand =
  def
    { createApplicationCommandName = "subtest",
      createApplicationCommandDescription = "testing out subcommands",
      createApplicationCommandOptions =
        Just
          [ def
              { applicationCommandOptionType = ApplicationCommandOptionTypeSubcommandGroup,
                applicationCommandOptionDescription = "the sub command group",
                applicationCommandOptionName = "frstsubcmdgrp",
                applicationCommandOptionOptions =
                  Just
                    [ def
                        { applicationCommandOptionType = ApplicationCommandOptionTypeSubcommand,
                          applicationCommandOptionDescription = "the first sub command",
                          applicationCommandOptionName = "frstsubcmd",
                          applicationCommandOptionOptions =
                            Just
                              [ def
                                  { applicationCommandOptionName = "onestringinput",
                                    applicationCommandOptionDescription = "two options",
                                    applicationCommandOptionRequired = Just True,
                                    applicationCommandOptionChoices =
                                      Just
                                        [ ApplicationCommandOptionChoice "yellow" (StringNumberValueString "yellow"),
                                          ApplicationCommandOptionChoice "blue" (StringNumberValueString "blue")
                                        ]
                                  },
                                def
                                  { applicationCommandOptionName = "oneintinput",
                                    applicationCommandOptionDescription = "choices galore",
                                    applicationCommandOptionType = ApplicationCommandOptionTypeInteger
                                  }
                              ]
                        }
                    ]
              },
            def
              { applicationCommandOptionType = ApplicationCommandOptionTypeSubcommand,
                applicationCommandOptionDescription = "the first sub command",
                applicationCommandOptionName = "frstsubcmd",
                applicationCommandOptionOptions =
                  Just
                    [ def
                        { applicationCommandOptionName = "onestringinput",
                          applicationCommandOptionDescription = "two options",
                          applicationCommandOptionRequired = Just True,
                          applicationCommandOptionChoices =
                            Just
                              [ ApplicationCommandOptionChoice "yellow" (StringNumberValueString "yellow"),
                                ApplicationCommandOptionChoice "blue" (StringNumberValueString "blue")
                              ]
                        },
                      def
                        { applicationCommandOptionName = "oneintinput",
                          applicationCommandOptionDescription = "choices galore",
                          applicationCommandOptionType = ApplicationCommandOptionTypeInteger
                        }
                    ]
              },
            def
              { applicationCommandOptionType = ApplicationCommandOptionTypeSubcommand,
                applicationCommandOptionDescription = "the second sub command",
                applicationCommandOptionName = "sndsubcmd",
                applicationCommandOptionOptions =
                  Just
                    [ def
                        { applicationCommandOptionName = "trueorfalse",
                          applicationCommandOptionDescription = "true or false",
                          applicationCommandOptionRequired = Just True,
                          applicationCommandOptionType = ApplicationCommandOptionTypeBoolean
                        },
                      def
                        { applicationCommandOptionName = "oneuserinput",
                          applicationCommandOptionDescription = "who is chosen",
                          applicationCommandOptionRequired = Just True,
                          applicationCommandOptionType = ApplicationCommandOptionTypeUser
                        }
                    ]
              },
            def
              { applicationCommandOptionName = "randominput",
                applicationCommandOptionDescription = "I shall not",
                applicationCommandOptionChoices =
                  Just
                    [ ApplicationCommandOptionChoice "firstopt" (StringNumberValueString "yay"),
                      ApplicationCommandOptionChoice "secondopt" (StringNumberValueString "nay")
                    ]
              }
          ]
    }

-- | An example of the example slash command using the defaults. Is equivalent
-- to `exampleSlashCommand`
exampleSlashCommand' :: CreateApplicationCommand
exampleSlashCommand' =
  def
    { createApplicationCommandName = "test",
      createApplicationCommandDescription = "here is a description",
      createApplicationCommandOptions = Just [h]
    }
  where
    h =
      def
        { applicationCommandOptionName = "randominput",
          applicationCommandOptionDescription = "I shall not",
          applicationCommandOptionRequired = Just True,
          applicationCommandOptionChoices =
            Just
              [ ApplicationCommandOptionChoice "firstopt" (StringNumberValueString "yay"),
                ApplicationCommandOptionChoice "secondopt" (StringNumberValueString "nay")
              ]
        }

-- | An example slash command.
exampleSlashCommand :: CreateApplicationCommand
exampleSlashCommand =
  CreateApplicationCommand
    "test"
    "here is a description"
    ( Just
        [ ApplicationCommandOption
            ApplicationCommandOptionTypeString
            "randominput"
            "I shall not"
            (Just True)
            ( Just
                [ ApplicationCommandOptionChoice "firstopt" (StringNumberValueString "yay"),
                  ApplicationCommandOptionChoice "secondopt" (StringNumberValueString "nay")
                ]
            )
            Nothing
            Nothing
            Nothing
            Nothing
            Nothing
        ]
    )
    Nothing
    Nothing

exampleInteractionResponse :: InteractionDataApplicationCommandChatInputOption -> InteractionResponse
exampleInteractionResponse d@(InteractionDataApplicationCommandChatInputOptionValues [InteractionDataApplicationCommandChatInputOptionValue {interactionDataApplicationCommandChatInputOptionValueValue = ApplicationCommandInteractionDataValueString s}]) =
  InteractionResponse
    InteractionCallbackTypeChannelMessageWithSource
    ( Just
        ( InteractionCallbackDataMessages
            ( interactionCallbackMessagesBasic
                ( T.pack $
                    "Here's the reply! You chose: "
                      ++ show s
                )
            )
        )
    )
exampleInteractionResponse _ =
  InteractionResponse
    InteractionCallbackTypeChannelMessageWithSource
    ( Just
        ( InteractionCallbackDataMessages
            ( interactionCallbackMessagesBasic
                "Something unexpected happened - the value was not what I expected!"
            )
        )
    )

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
                      ( ComponentActionSelectMenu
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
  -- void $ restCall (R.CreateMessageDetailed (messageChannelId m) opts)
  Ready _ _ _ _ _ _ pa@(PartialApplication i _) ->
    void $
      restCall (R.CreateGuildApplicationCommand i testserverid exampleSlashCommand) >> restCall (R.CreateGuildApplicationCommand i testserverid exampleUserCommand) >> restCall (R.CreateGuildApplicationCommand i testserverid newExampleSlashCommand)
  -- InteractionCreate
  --   i@InternalInteraction
  --     { internalInteractionType = InteractionTypeApplicationCommand,
  --       internalInteractionData = Just InternalInteractionData {internalInteractionDataApplicationCommandType = (Just ApplicationCommandTypeUser), internalInteractionDataApplicationCommandName = Just "usercomm"},
  --       ..
  --     } ->
  --     void $
  --       restCall (R.CreateInteractionResponse internalInteractionId internalInteractionToken (InteractionResponse InteractionCallbackTypeDeferredChannelMessageWithSource Nothing))
  --         >> restCall
  --           ( R.EditOriginalInteractionResponse
  --               internalInteractionApplicationId
  --               internalInteractionToken
  --               (interactionCallbackMessagesBasic $ T.pack $ "You clicked on " <> show (internalInteractionData i >>= \d -> internalInteractionDataResolved d))
  --           )
  InteractionCreate InteractionApplicationCommand 
  InteractionCreate InteractionApplicationCommand {interactionChannelId = Just cid, interactionDataApplicationCommand = Just InteractionDataApplicationCommandChatInput {interactionDataApplicationCommandName = "test", interactionDataApplicationCommandOptions = Just d, ..}, ..} ->
    void $
      restCall
        (R.CreateInteractionResponse interactionId interactionToken (exampleInteractionResponse d))
  -- case interactionChannelId of
  --   Nothing -> return ()
  --   Just cid -> do
  --       case interactionDataApplicationCommand of
  --         Nothing -> return ()
  --         Just d -> do
  --           case internalInteractionDataApplicationCommandType d of
  --             (Just ApplicationCommandTypeChatInput) -> case internalInteractionDataOptions d of
  --               Nothing -> return ()
  --               Just d' ->
  --                 void $
  --                   restCall
  --                     (R.CreateInteractionResponse internalInteractionId internalInteractionToken (exampleInteractionResponse d'))
  --             _ -> void $ restCall (R.CreateMessage cid "I got some other kind of application command!")
  -- Note that the above is not the required way of receiving and replying to interactions - this is marked as a failure

  e -> trace ("uncaught:" ++ show e) $ return ()

isTextChannel :: Channel -> Bool
isTextChannel (ChannelText {}) = True
isTextChannel _ = False

fromBot :: Message -> Bool
fromBot = userIsBot . messageAuthor

isPing :: Message -> Bool
isPing = ("ping" `T.isPrefixOf`) . T.toLower . messageContent
