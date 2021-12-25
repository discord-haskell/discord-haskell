-- allows "strings" to be Data.Text
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

import Control.Monad (forM_, void, when)
import Data.Char (isDigit)
import Data.List (transpose)
import Data.Maybe (fromJust, isNothing)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Discord
import qualified Discord.Requests as R
import Discord.Types
import qualified Network.HTTP.Req as RH
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
  err <-
    runDiscord $
      def
        { discordToken = tok,
          discordOnStart = startHandler,
          discordOnEnd = liftIO $ putStrLn "Ended",
          discordOnEvent = eventHandler,
          discordOnLog = \s -> TIO.putStrLn s >> TIO.putStrLn ""
        }

  -- only reached on an unrecoverable error
  -- put normal 'cleanup' code in discordOnEnd
  TIO.putStrLn err

-- If the start handler throws an exception, discord-haskell will gracefully shutdown
--     Use place to execute commands you know you want to complete
startHandler :: DiscordHandler ()
startHandler = do
  -- Right partialGuilds <- restCall R.GetCurrentUserGuilds

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
exampleInteractionResponse (InteractionDataApplicationCommandOptionsValues [InteractionDataApplicationCommandOptionValue {interactionDataApplicationCommandOptionValueValue = ApplicationCommandInteractionDataValueString s}]) =
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
                            ComponentButtonUrl
                              (RH.https "github.com" RH./: "aquarial" RH./: "discord-haskell")
                              False
                              "Button 3"
                              Nothing
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
        tictactoe :: R.MessageDetailedOpts
        tictactoe =
          def
            { R.messageDetailedContent = "Playing tic tac toe! Player 0",
              R.messageDetailedComponents = Just $ updateTicTacToe Nothing []
            }
    void $ restCall (R.CreateMessageDetailed (messageChannelId m) opts')
    void $ restCall (R.CreateMessageDetailed (messageChannelId m) tictactoe)
  Ready _ _ _ _ _ _ (PartialApplication i _) ->
    mapM_ (maybe (return ()) (void . restCall . R.CreateGuildApplicationCommand i testserverid)) [Just exampleSlashCommand, exampleUserCommand, newExampleSlashCommand]
  InteractionCreate InteractionComponent {interactionDataComponent = Just cb@InteractionDataComponentButton {interactionDataComponentCustomId = (T.take 3 -> "ttt")}, ..} -> case processTicTacToe cb interactionMessage of
    [r] ->
      void
        ( restCall
            ( R.CreateInteractionResponse
                interactionId
                interactionToken
                ( InteractionResponse
                    InteractionCallbackTypeUpdateMessage
                    ( Just
                        (InteractionCallbackDataMessages r)
                    )
                )
            )
        )
    r : rs ->
      void
        ( restCall $
            R.CreateInteractionResponse
              interactionId
              interactionToken
              ( InteractionResponse
                  InteractionCallbackTypeUpdateMessage
                  ( Just
                      (InteractionCallbackDataMessages r)
                  )
              )
        )
        >> mapM_
          ( restCall
              . R.CreateFollowupInteractionMessage
                interactionApplicationId
                interactionToken
          )
          rs
    _ -> return ()
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

processTicTacToe :: InteractionDataComponent -> Message -> [InteractionCallbackMessages]
processTicTacToe (InteractionDataComponentButton cid) m = case messageComponents m of
  Nothing -> [interactionCallbackMessagesBasic "Sorry, I couldn't get the components on that message."]
  (Just cs) ->
    let newComp = newComp' cs
     in ( ( interactionCallbackMessagesBasic
              ("Some Tic Tac Toe! Player " <> (if '0' == T.last (messageContent m) then "1" else "0"))
          )
            { interactionCallbackMessagesComponents = Just ((if checkTicTacToe newComp then (disableAll <$>) else id) newComp)
            }
        ) :
          [interactionCallbackMessagesBasic ("Player " <> T.singleton player <> " has won!") | checkTicTacToe newComp]
  where
    player = T.last (messageContent m)
    newComp' = updateTicTacToe (Just (cid, '0' == player))
    disableAll c@Component {componentType = ComponentTypeActionRow, componentComponents = Just cs, ..} = c {componentComponents = Just (disableAll <$> cs)}
    disableAll c = c {componentDisabled = Just True}
processTicTacToe _ _ = [interactionCallbackMessagesBasic "Sorry, I couldn't understand that button."]

checkTicTacToe :: [Component] -> Bool
checkTicTacToe xs = checkRows unwrapped || checkRows unwrappedT || checkRows [diagonal unwrapped, diagonal (reverse <$> unwrapped)]
  where
    checkRows = any (\cbs -> all (\cb -> cb == head cbs && cb /= InternalButtonStyleSecondary) cbs)
    unwrapped = (\Component {componentComponents = Just cbs} -> (\Component {componentStyle = Just style} -> style) <$> cbs) <$> xs
    unwrappedT = transpose unwrapped
    diagonal [] = []
    diagonal ([] : _) = []
    diagonal (ys : yss) = head ys : diagonal (tail <$> yss)

updateTicTacToe :: Maybe (T.Text, Bool) -> [Component] -> [Component]
updateTicTacToe Nothing _ = (\y -> toInternal (ComponentActionRowButton $ (\x -> ComponentButton (T.pack $ "ttt " <> show x <> show y) False ButtonStyleSecondary "[ ]" Nothing) <$> [0 .. 4])) <$> [0 .. 4]
updateTicTacToe (Just (tttxy, isFirst)) car
  | isNothing car' || not (checkIsValid tttxy) = car
  | otherwise = (\(ComponentActionRowButton cbs) -> toInternal $ ComponentActionRowButton (changeIf <$> cbs)) <$> fromJust car'
  where
    car' = mapM fromInternal car :: Maybe [ComponentActionRow]
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
