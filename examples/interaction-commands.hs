{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

import Control.Monad (forM_, when)
import qualified Data.ByteString as B
import Data.Char (isDigit)
import Data.Functor ((<&>))
import Data.List (transpose)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Discord
import Discord.Interactions
import qualified Discord.Requests as R
import Discord.Types
import UnliftIO (liftIO)
import UnliftIO.Concurrent

main :: IO ()
main =
  if testserverid == -1
    then TIO.putStrLn "ERROR: modify the source and set testserverid to your serverid"
    else interactionCommandExample

testserverid :: Snowflake
testserverid = 463428416008355872

void :: DiscordHandler (Either RestCallErrorCode b) -> DiscordHandler ()
void =
  ( >>=
      ( \case
          Left e -> liftIO $ print e
          Right _ -> return ()
      )
  )

-- | Replies "pong" to every message that starts with "ping"
interactionCommandExample :: IO ()
interactionCommandExample = do
  tok <- TIO.readFile "./examples/auth-token.secret"

  -- open ghci and run  [[ :info RunDiscordOpts ]] to see available fields
  t <-
    runDiscord $
      def
        { discordToken = tok,
          discordOnStart = startHandler,
          discordOnEnd = liftIO $ putStrLn "Ended",
          discordOnEvent = eventHandler,
          discordOnLog = \s -> TIO.putStrLn s >> TIO.putStrLn "",
          discordGatewayIntent = def {gatewayIntentMembers = True, gatewayIntentPrecenses = True}
        }
  TIO.putStrLn t

-- If the start handler throws an exception, discord-haskell will gracefully shutdown
--     Use place to execute commands you know you want to complete
startHandler :: DiscordHandler ()
startHandler = do
  let activity =
        def
          { activityName = "ping-pong",
            activityType = ActivityTypeGame
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
                ( ApplicationCommandOptionsSubcommands
                    [ ApplicationCommandOptionSubcommandGroup
                        "frstsubcmdgrp"
                        "the sub command group"
                        [ ApplicationCommandOptionSubcommand
                            "frstsubcmd"
                            "the first sub sub command"
                            [ ApplicationCommandOptionValueString
                                "onestringinput"
                                "two options"
                                True
                                ( Right
                                    [ Choice "green" "green",
                                      Choice "red" "red"
                                    ]
                                ),
                              ApplicationCommandOptionValueInteger "oneintinput" "choices galore" False (Left False) Nothing Nothing
                            ]
                        ],
                      ApplicationCommandOptionSubcommandOrGroupSubcommand $
                        ApplicationCommandOptionSubcommand
                          "frstsubcmd"
                          "the first subcommand"
                          [ ApplicationCommandOptionValueString
                              "onestringinput"
                              "two options"
                              True
                              ( Right
                                  [ Choice "yellow" "yellow",
                                    Choice "blue" "blue"
                                  ]
                              )
                          ],
                      ApplicationCommandOptionSubcommandOrGroupSubcommand $
                        ApplicationCommandOptionSubcommand
                          "sndsubcmd"
                          "the second subcommand"
                          [ ApplicationCommandOptionValueBoolean
                              "trueorfalse"
                              "true or false"
                              True,
                            ApplicationCommandOptionValueNumber
                              "numbercomm"
                              "number option"
                              False
                              (Left True)
                              (Just 3.1415)
                              (Just 101),
                            ApplicationCommandOptionValueInteger
                              "numbercomm2"
                              "another number option"
                              False
                              (Right [Choice "one" 1, Choice "two" 2, Choice "minus 1" (-1)])
                              (Just $ -1)
                              (Just $ -2),
                            ApplicationCommandOptionValueInteger
                              "numbercomm3"
                              "another another number option"
                              False
                              (Left True)
                              (Just $ -50)
                              (Just 50),
                            ApplicationCommandOptionValueUser
                              "user"
                              "testing asking for a user"
                              False,
                            ApplicationCommandOptionValueChannel
                              "channel"
                              "testing asking for a channel"
                              False
                              (Just [ApplicationCommandChannelTypeGuildVoice]),
                            ApplicationCommandOptionValueMentionable
                              "mentionable"
                              "testing asking for a mentionable"
                              False
                          ]
                    ]
                )
          }

-- | An example slash command.
exampleSlashCommand :: Maybe CreateApplicationCommand
exampleSlashCommand =
  createApplicationCommandChatInput
    "test"
    "here is a description"
    >>= \cac ->
      return $
        cac
          { createApplicationCommandOptions =
              Just $
                ApplicationCommandOptionsValues
                  [ ApplicationCommandOptionValueString
                      "randominput"
                      "I shall not"
                      True
                      (Right [Choice "firstOpt" "yay", Choice "secondOpt" "nay"])
                  ]
          }

exampleInteractionResponse :: InteractionDataApplicationCommandOptions -> InteractionResponse
exampleInteractionResponse (InteractionDataApplicationCommandOptionsValues [InteractionDataApplicationCommandOptionValueString {interactionDataApplicationCommandOptionValueStringValue = s}]) =
  interactionResponseBasic (T.pack $ "Here's the reply! You chose: " ++ show s)
exampleInteractionResponse _ =
  interactionResponseBasic
    "Something unexpected happened - the value was not what I expected!"

getImage :: IO B.ByteString
getImage = return "\137PNG\r\n\SUB\n\NUL\NUL\NUL\rIHDR\NUL\NUL\NUL\SOH\NUL\NUL\NUL\SOH\b\STX\NUL\NUL\NUL\144wS\222\NUL\NUL\NUL\SOHsRGB\NUL\174\206\FS\233\NUL\NUL\NUL\EOTgAMA\NUL\NUL\177\143\v\252a\ENQ\NUL\NUL\NUL\tpHYs\NUL\NUL\SO\195\NUL\NUL\SO\195\SOH\199o\168d\NUL\NUL\NUL\fIDAT\CANWc\248\239\180\t\NUL\EOT7\SOH\244\162\155\ENQ\235\NUL\NUL\NUL\NULIEND\174B`\130"


-- If an event handler throws an exception, discord-haskell will continue to run
eventHandler :: Event -> DiscordHandler ()
eventHandler event = case event of
  MessageCreate m -> when (not (fromBot m) && isPing m) $ do
    void $ restCall (R.CreateReaction (messageChannelId m, messageId m) "eyes")
    threadDelay (2 * 10 ^ (6 :: Int))

    -- A very simple message.
    void $ restCall (R.CreateMessage (messageChannelId m) "Pong!")
    exampleImage <- liftIO getImage

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
                  [ ComponentActionRowButton
                      [ ComponentButton "Button 1" False ButtonStylePrimary "Button 1" (Just (mkEmoji "🔥")),
                        ComponentButton "Button 2" True ButtonStyleSuccess "Button 2" Nothing,
                        ComponentButtonUrl
                          "https://github.com/aquarial/discord-haskell"
                          False
                          "Button 3"
                          Nothing
                      ],
                    ComponentActionRowSelectMenu
                      ( ComponentSelectMenu
                          "action select menu"
                          False
                          [ SelectOption "First option" "opt1" (Just "the only desc") Nothing Nothing,
                            SelectOption "Second option" "opt2" Nothing (Just (mkEmoji "😭")) (Just True),
                            SelectOption "third option" "opt3" Nothing Nothing Nothing,
                            SelectOption "fourth option" "opt4" Nothing Nothing Nothing,
                            SelectOption "fifth option" "opt5" Nothing Nothing Nothing
                          ]
                          (Just "this is a place holder")
                          (Just 2)
                          (Just 5)
                      )
                  ],
              R.messageDetailedEmbeds =
                Just
                  [ def
                      { createEmbedTitle = "Title",
                        createEmbedDescription = "the description",
                        createEmbedAuthorName = "Author name is Required",
                        createEmbedImage = Just (CreateEmbedImageUrl "https://media.discordapp.net/attachments/365969021083975681/936055590415921172/Warning.png"),
                        createEmbedColor = Just DiscordColorLuminousVividPink,
                        createEmbedAuthorIcon = Just (CreateEmbedImageUpload exampleImage)
                      },
                    def {createEmbedTitle = "a different Title", createEmbedDescription = "another desc"}
                  ]
            }
        tictactoe :: R.MessageDetailedOpts
        tictactoe =
          def
            { R.messageDetailedContent = "Playing tic tac toe! Player 0",
              R.messageDetailedComponents = Just $ updateTicTacToe Nothing []
            }
    void $ restCall (R.CreateMessageDetailed (messageChannelId m) opts')
    void $ restCall (R.CreateMessageDetailed (messageChannelId m) tictactoe)
  Ready _ _ _ _ _ _ (PartialApplication i _) -> do
    vs <-
      mapM
        (maybe (return (Left $ RestCallErrorCode 0 "" "")) (restCall . R.CreateGuildApplicationCommand i testserverid))
        [exampleSlashCommand, exampleUserCommand, newExampleSlashCommand, createApplicationCommandChatInput "modal" "modal test"]
    liftIO (putStrLn $ "number of application commands added " ++ show (length vs))
    acs <- restCall (R.GetGuildApplicationCommands i testserverid)
    case acs of
      Left r -> liftIO $ print r
      Right ls -> liftIO $ putStrLn $ "number of application commands total " ++ show (length ls)
  InteractionCreate InteractionComponent {interactionDataComponent = cb@InteractionDataComponentButton {interactionDataComponentCustomId = (T.take 3 -> "ttt")}, ..} -> case processTicTacToe cb interactionMessage of
    [r] ->
      void
        ( restCall
            ( R.CreateInteractionResponse
                interactionId
                interactionToken
                (InteractionResponseUpdateMessage r)
            )
        )
    r : rs ->
      void
        ( restCall $
            R.CreateInteractionResponse
              interactionId
              interactionToken
              (InteractionResponseUpdateMessage r)
        )
        >> mapM_
          ( restCall
              . R.CreateFollowupInteractionMessage
                interactionApplicationId
                interactionToken
          )
          rs
    _ -> return ()
  InteractionCreate InteractionApplicationCommand {interactionDataApplicationCommand = InteractionDataApplicationCommandUser {interactionDataApplicationCommandName = nm, interactionDataApplicationCommandTargetId = uid, ..}, ..} ->
    void $
      restCall
        (R.CreateInteractionResponse interactionId interactionToken (interactionResponseBasic $ "Command " <> nm <> T.pack (" selected user: " ++ show uid)))
  InteractionCreate InteractionApplicationCommand {interactionDataApplicationCommand = InteractionDataApplicationCommandChatInput {interactionDataApplicationCommandName = "test", interactionDataApplicationCommandOptions = Just d, ..}, ..} ->
    void $
      restCall
        (R.CreateInteractionResponse interactionId interactionToken (exampleInteractionResponse d))
  InteractionCreate InteractionApplicationCommand {interactionDataApplicationCommand = InteractionDataApplicationCommandChatInput {interactionDataApplicationCommandName = "subtest", interactionDataApplicationCommandOptions = Just d, ..}, ..} -> void $ restCall (R.CreateInteractionResponse interactionId interactionToken (interactionResponseBasic (T.pack $ "oh boy, subcommands! welp, here's everything I got from that: " <> show d)))
  InteractionCreate InteractionComponent {interactionDataComponent = InteractionDataComponentButton {..}, ..} ->
    void $
      restCall
        ( R.CreateInteractionResponse interactionId interactionToken $
            InteractionResponseChannelMessage
              ( ( interactionResponseMessageBasic $ "You pressed the button " <> interactionDataComponentCustomId
                )
                  { interactionResponseMessageFlags = Just (InteractionResponseMessageFlags [InteractionResponseMessageFlagEphermeral])
                  }
              )
        )
  InteractionCreate InteractionComponent {interactionDataComponent = InteractionDataComponentSelectMenu {interactionDataComponentValues = vs}, ..} ->
    void
      ( do
          exampleImage <- liftIO getImage
          aid <- readCache <&> cacheApplication <&> partialApplicationID
          _ <- restCall (R.CreateInteractionResponse interactionId interactionToken InteractionResponseDeferChannelMessage)
          restCall
            ( R.CreateFollowupInteractionMessage
                aid
                interactionToken
                (interactionResponseMessageBasic (T.pack $ "oh dear, select menu. thank you for waiting" <> show vs))
                  { interactionResponseMessageEmbeds =
                      Just
                        [ def
                            { createEmbedTitle = "Select menu title",
                              createEmbedDescription = "Here is the select menu embed desc",
                              createEmbedAuthorName = "someunknownentity",
                              createEmbedImage = Just (CreateEmbedImageUrl "https://media.discordapp.net/attachments/365969021083975681/936055590415921172/Warning.png"),
                              createEmbedColor = Just DiscordColorDiscordBlurple,
                              createEmbedAuthorIcon = Just (CreateEmbedImageUpload exampleImage)
                            }
                        ]
                  }
            )
      )
  InteractionCreate InteractionApplicationCommandAutocomplete {interactionDataApplicationCommand = InteractionDataApplicationCommandChatInput {interactionDataApplicationCommandName = "subtest", interactionDataApplicationCommandOptions = Just _, ..}, ..} -> void (restCall $ R.CreateInteractionResponse interactionId interactionToken (InteractionResponseAutocompleteResult (InteractionResponseAutocompleteInteger [Choice "five" 5])))
  InteractionCreate i@InteractionApplicationCommand {interactionDataApplicationCommand = InteractionDataApplicationCommandChatInput {interactionDataApplicationCommandName = "modal"}} ->
    void $
      restCall
        ( R.CreateInteractionResponse
            (interactionId i)
            (interactionToken i)
            ( InteractionResponseModal
                ( InteractionResponseModalData
                    "customidmodal"
                    "modal title"
                    [mkComponentTextInput "textcid" "textlabel"]
                )
            )
        )
  InteractionCreate i@InteractionModalSubmit {interactionDataModal = idm} -> void $ restCall (R.CreateInteractionResponse (interactionId i) (interactionToken i) (interactionResponseBasic (T.pack (show idm))))
  _ -> return ()

processTicTacToe :: InteractionDataComponent -> Message -> [InteractionResponseMessage]
processTicTacToe (InteractionDataComponentButton cid) m = case messageComponents m of
  Nothing -> [interactionResponseMessageBasic "Sorry, I couldn't get the components on that message."]
  (Just cs) ->
    let newComp = newComp' cs
     in ( ( interactionResponseMessageBasic
              ("Some Tic Tac Toe! Player " <> (if '0' == T.last (messageContent m) then "1" else "0"))
          )
            { interactionResponseMessageComponents = Just ((if checkTicTacToe newComp then (disableAll <$>) else id) newComp)
            }
        ) :
          [interactionResponseMessageBasic ("Player " <> T.singleton player <> " has won!") | checkTicTacToe newComp]
  where
    player = T.last (messageContent m)
    newComp' = updateTicTacToe (Just (cid, '0' == player))
    disableAll (ComponentActionRowButton cs) = ComponentActionRowButton $ (\c -> c {componentButtonDisabled = True}) <$> cs
    disableAll c = c
processTicTacToe _ _ = [interactionResponseMessageBasic "Sorry, I couldn't understand that button."]

checkTicTacToe :: [ComponentActionRow] -> Bool
checkTicTacToe xs = checkRows unwrapped || checkRows unwrappedT || checkRows [diagonal unwrapped, diagonal (reverse <$> unwrapped)]
  where
    checkRows = any (\cbs -> all (\cb -> cb == head cbs && cb /= ButtonStyleSecondary) cbs)
    unwrapped = (\(ComponentActionRowButton cbs) -> (\ComponentButton {componentButtonStyle = style} -> style) <$> cbs) <$> xs
    unwrappedT = transpose unwrapped
    diagonal [] = []
    diagonal ([] : _) = []
    diagonal (ys : yss) = head ys : diagonal (tail <$> yss)

updateTicTacToe :: Maybe (T.Text, Bool) -> [ComponentActionRow] -> [ComponentActionRow]
updateTicTacToe Nothing _ = (\y -> ComponentActionRowButton $ (\x -> ComponentButton (T.pack $ "ttt " <> show x <> show y) False ButtonStyleSecondary "[ ]" Nothing) <$> [0 .. 4]) <$> [0 .. 4]
updateTicTacToe (Just (tttxy, isFirst)) car
  | not (checkIsValid tttxy) = car
  | otherwise = (\(ComponentActionRowButton cbs) -> ComponentActionRowButton (changeIf <$> cbs)) <$> car
  where
    checkIsValid tttxy' = T.length tttxy' == 6 && all isDigit [T.index tttxy' 4, T.index tttxy' 5]
    getxy tttxy' = (T.index tttxy' 4, T.index tttxy' 5)
    (style, symbol) = if isFirst then (ButtonStyleSuccess, "[X]") else (ButtonStyleDanger, "[O]")
    changeIf cb@ComponentButton {..}
      | checkIsValid componentButtonCustomId && getxy tttxy == getxy componentButtonCustomId = cb {componentButtonDisabled = True, componentButtonStyle = style, componentButtonLabel = symbol}
      | otherwise = cb
    changeIf cb = cb

isTextChannel :: Channel -> Bool
isTextChannel ChannelText {} = True
isTextChannel _ = False

fromBot :: Message -> Bool
fromBot = userIsBot . messageAuthor

isPing :: Message -> Bool
isPing = ("ping" `T.isPrefixOf`) . T.toLower . messageContent
