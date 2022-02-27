{-# LANGUAGE OverloadedStrings #-}  -- allows "strings" to be Data.Text

import Control.Monad (when, forM_, void)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import UnliftIO (liftIO)
import UnliftIO.Concurrent

import Discord
import Discord.Types
import qualified Discord.Requests as R

-- Allows this code to be an executable. See discord-haskell.cabal
main :: IO ()
main = if testserverid == -1
       then TIO.putStrLn "ERROR: modify the source and set testserverid to your serverid"
       else pingpongExample



-- check the url in a discord server
--                                <server id>           <channel id>
-- https://discord.com/channels/2385235298674262408/4286572469284672046
testserverid :: Snowflake
testserverid = -1

-- | Replies "pong" to every message that starts with "ping"
pingpongExample :: IO ()
pingpongExample = do
  tok <- TIO.readFile "./examples/auth-token.secret"

  -- open ghci and run  [[ :info RunDiscordOpts ]] to see available fields
  err <- runDiscord $ def { discordToken = tok
                          , discordOnStart = startHandler
                          , discordOnEnd = liftIO $ putStrLn "Ended"
                          , discordOnEvent = eventHandler
                          , discordOnLog = \s -> TIO.putStrLn s >> TIO.putStrLn ""
                          , discordGatewayIntent = def {gatewayIntentMembers = True, gatewayIntentPrecenses =True}
                          }

  -- only reached on an unrecoverable error
  -- put normal 'cleanup' code in discordOnEnd
  TIO.putStrLn err

-- If the start handler throws an exception, discord-haskell will gracefully shutdown
--     Use place to execute commands you know you want to complete
startHandler :: DiscordHandler ()
startHandler = do
  let activity = def { activityName = "ping-pong"
                     , activityType = ActivityTypeGame
                     }
  let opts = UpdateStatusOpts { updateStatusOptsSince = Nothing
                              , updateStatusOptsGame = Just activity
                              , updateStatusOptsNewStatus = UpdateStatusOnline
                              , updateStatusOptsAFK = False
                              }
  sendCommand (UpdateStatus opts)

  Right chans <- restCall $ R.GetGuildChannels testserverid
  forM_ (take 1 (filter isTextChannel chans))
        (\channel -> restCall $ R.CreateMessage (channelId channel)
                        "Hello! I will reply to pings with pongs")


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
            opts = def { R.messageDetailedContent = "Here's a more complex message, but doesn't ping @everyone!"
                       , R.messageDetailedTTS = True
                       , R.messageDetailedAllowedMentions = Just $
                          def { R.mentionEveryone = False
                              , R.mentionRepliedUser = False
                              }
                       , R.messageDetailedReference = Just $
                          def { referenceMessageId = Just $ messageId m }
                       }
        void $ restCall (R.CreateMessageDetailed (messageChannelId m) opts)
      _ -> return ()

isTextChannel :: Channel -> Bool
isTextChannel (ChannelText {}) = True
isTextChannel _ = False

fromBot :: Message -> Bool
fromBot = userIsBot . messageAuthor

isPing :: Message -> Bool
isPing = ("ping" `T.isPrefixOf`) . T.toLower . messageContent
