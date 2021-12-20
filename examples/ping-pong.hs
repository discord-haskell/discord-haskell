-- allows "strings" to be Data.Text
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

import Control.Monad (forM_, void, when)
import Data.Maybe (fromJust)
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

exampleInteractionResponse :: Maybe [ApplicationCommandInteractionDataOption] -> InteractionResponse
exampleInteractionResponse d =
  InteractionResponse
    InteractionCallbackTypeChannelMessageWithSource
    ( Just
        ( InteractionCallbackDataMessages
            ( InteractionCallbackMessages
                Nothing
                ( Just
                    ( T.pack $
                        "Here's the reply! You chose: "
                          ++ show (fromJust $ applicationCommandInteractionDataOptionValue $ head (fromJust d))
                    )
                )
                Nothing
                Nothing
                Nothing
                Nothing
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
    trace
      (show pa)
      ( restCall
          ( R.CreateGuildApplicationCommand i testserverid exampleSlashCommand
          )
      )
      >>= \rs -> trace (show rs) (return ())
  InteractionCreate i@Interaction {..} -> do
    let cid = fromJust interactionChannelId
    if interactionType /= InteractionTypeApplicationCommand
      then void $ restCall (R.CreateMessage cid (T.pack $ "I don't know how to handle an interaction like that! " <> (show interactionType)))
      else do
        let d = fromJust interactionData
        case interactionDataApplicationCommandType d of
          (Just ApplicationCommandTypeChatInput) ->
            void $
              restCall
                ( R.CreateInteractionResponse interactionId interactionToken (exampleInteractionResponse (interactionDataOptions d))
                )
          _ -> void $ restCall (R.CreateMessage cid "I got some other kind of application command!")
  -- Note that the above is not the required way of receiving and replying to interactions - this is marked as a failure

  _ -> return ()

isTextChannel :: Channel -> Bool
isTextChannel (ChannelText {}) = True
isTextChannel _ = False

fromBot :: Message -> Bool
fromBot = userIsBot . messageAuthor

isPing :: Message -> Bool
isPing = ("ping" `T.isPrefixOf`) . T.toLower . messageContent
