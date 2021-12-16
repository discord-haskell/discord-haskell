{-# LANGUAGE OverloadedStrings #-}  -- allows "strings" to be Data.Text
{-# LANGUAGE  RecordWildCards #-}

import Control.Monad (when, forM_, void)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import UnliftIO (liftIO)
import UnliftIO.Concurrent

import Discord
import Discord.Types
import qualified Discord.Requests as R
import Debug.Trace
import Data.Maybe (fromJust)
import qualified Discord.Internal.Rest.ApplicationCommands as R
import Discord.Internal.Types.ApplicationCommands (InteractionResponse(InteractionResponse))
import Data.Aeson

-- Allows this code to be an executable. See discord-haskell.cabal
main :: IO ()
main = pingpongExample

-- | Replies "pong" to every message that starts with "ping"
pingpongExample :: IO ()
pingpongExample = do
  tok <- TIO.readFile "./examples/auth-token.secret"

  -- open ghci and run  [[ :info RunDiscordOpts ]] to see available fields
  t <- runDiscord $ def { discordToken = tok
                        , discordOnStart = startHandler
                        , discordOnEnd = liftIO $ putStrLn "Ended"
                        , discordOnEvent = eventHandler
                        , discordOnLog = \s -> TIO.putStrLn s >> TIO.putStrLn ""
                        }
  TIO.putStrLn t

-- If the start handler throws an exception, discord-haskell will gracefully shutdown
--     Use place to execute commands you know you want to complete
startHandler :: DiscordHandler ()
startHandler = do
  Right partialGuilds <- restCall R.GetCurrentUserGuilds

  let activity = Activity { activityName = "ping-pong"
                          , activityType = ActivityTypeGame
                          , activityUrl = Nothing
                          }
  let opts = UpdateStatusOpts { updateStatusOptsSince = Nothing
                              , updateStatusOptsGame = Just activity
                              , updateStatusOptsNewStatus = UpdateStatusOnline
                              , updateStatusOptsAFK = False
                              }
  sendCommand (UpdateStatus opts)

  -- forM_ partialGuilds $ \pg -> do
  --   Right guild <- restCall $ R.GetGuild (partialGuildId pg)
  --   Right chans <- restCall $ R.GetGuildChannels (guildId guild)
  --   forM_ (take 1 (filter isTextChannel chans))
  --     (\channel -> restCall $ R.CreateMessage (channelId channel)
  --                                     "Hello! I will reply to pings with pongs")


-- If an event handler throws an exception, discord-haskell will continue to run
eventHandler :: Event -> DiscordHandler ()
eventHandler event = case event of
      MessageCreate m -> when (not (fromBot m) && isPing m) $ do
        void $ restCall (R.CreateReaction (messageChannel m, messageId m) "eyes")
        threadDelay (2 * 10^(6 :: Int))

        -- A very simple message.
        void $ restCall (R.CreateMessage (messageChannel m) "Pong!")

        -- A more complex message. Text-to-speech, does not mention everyone nor
        -- the user, and uses Discord native replies.
        -- Use ":info" in ghci to explore the type
        let opts :: R.MessageDetailedOpts
            opts = def { R.messageDetailedContent = "Here's a more complex message, but doesn't ping @everyone!"
                       , R.messageDetailedTTS = True
                       , R.messageDetailedAllowedMentions = Just $
                          def { R.mentionEveryone = False
                              , R.mentionRepliedUser = False
                              }
                       , R.messageDetailedReference = Just $
                          def { referenceMessageId = Just $ messageId m }
                       }
        void $ restCall (R.CreateMessageDetailed (messageChannel m) opts)
      Ready _ _ _ _ _ _ pa@(PartialApplication i f) -> 
        trace (show pa) (
          restCall (
            R.CreateGuildApplicationCommand i serverid (
              CreateApplicationCommand 
                "test" 
                "here is a description" 
                (Just 
                  [
                    ApplicationCommandOption 
                      STRING 
                      "randominput" 
                      "I shall not" 
                      (Just True) 
                      (Just 
                        [
                          ApplicationCommandOptionChoice "firstopt" (SIDS "yay"), 
                          ApplicationCommandOptionChoice "secondopt" (SIDS "nay")
                        ]
                      ) Nothing Nothing Nothing Nothing Nothing 
                  ]
                ) 
                Nothing
                Nothing
            ) 
          )
        ) >>= \rs -> trace (show rs) (return ())
      InteractionCreate i@(Interaction {..}) ->  trace (show i) $ do
        let cid = fromJust interactionChannelId
        if not (interactionType == APPLICATION_COMMAND) then void $ restCall (R.CreateMessage cid "I don't know how to handle an interaction like that!")
        else do
          let d = fromJust interactionData
          case interactionDataApplicationCommandType d of
            ACTCHAT_INPUT -> void $  restCall (
                let x = (InteractionResponse 
                            CHANNEL_MESSAGE_WITH_SOURCE 
                            (Just 
                              (ICDM 
                                (InteractionCallbackMessages 
                                  Nothing 
                                  (Just 
                                    (T.pack $ "Here's the reply! You chose: " 
                                      ++ (show $ fromJust $ applicationCommandInteractionDataOptionValue $ head  (fromJust $ interactionDataOptions d))
                                    )
                                  )
                                  Nothing
                                  Nothing
                                  Nothing
                                  Nothing
                                )
                              )
                            )
                          ) in trace (show (toJSON x)) (R.CreateInteractionResponse interactionId interactionToken x)
                )
            _ -> void $ restCall (R.CreateMessage cid "I got some other kind of application command!")
      -- Note that the above is not the required way of receiving and replying to interactions - this is marked as a failure

      _ -> return ()
  where
    serverid = -1

isTextChannel :: Channel -> Bool
isTextChannel (ChannelText {}) = True
isTextChannel _ = False

fromBot :: Message -> Bool
fromBot = userIsBot . messageAuthor

isPing :: Message -> Bool
isPing = ("ping" `T.isPrefixOf`) . T.toLower . messageText
