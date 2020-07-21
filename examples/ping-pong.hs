{-# LANGUAGE OverloadedStrings #-}  -- allows "strings" to be Data.Text

import Control.Monad (when, forM_)
import Control.Concurrent (threadDelay)
import Data.Char (toLower)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Discord
import Discord.Types
import qualified Discord.Requests as R

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
                        , discordOnEnd = putStrLn "Ended"
                        , discordOnEvent = eventHandler
                        , discordOnLog = \s -> TIO.putStrLn s >> TIO.putStrLn ""
                        }
  threadDelay (1 `div` 10 * 10^(6 :: Int))
  TIO.putStrLn t

-- If the start handler throws an exception, discord-haskell will gracefully shutdown
--     Use place to execute commands you know you want to complete
startHandler :: DiscordHandle -> IO ()
startHandler dis = do
  Right partialGuilds <- restCall dis R.GetCurrentUserGuilds

  forM_ partialGuilds $ \pg -> do
    Right guild <- restCall dis $ R.GetGuild (partialGuildId pg)
    Right chans <- restCall dis $ R.GetGuildChannels (guildId guild)
    case filter isTextChannel chans of
      (c:_) -> do _ <- restCall dis $ R.CreateMessage (channelId c) "Hello! I will reply to pings with pongs"
                  pure ()
      _ -> pure ()

-- If an event handler throws an exception, discord-haskell will continue to run
eventHandler :: DiscordHandle -> Event -> IO ()
eventHandler dis event = case event of
      MessageCreate m -> when (not (fromBot m) && isPing m) $ do
        _ <- restCall dis (R.CreateReaction (messageChannel m, messageId m) "eyes")
        threadDelay (4 * 10^(6 :: Int))
        _ <- restCall dis (R.CreateMessage (messageChannel m) "Pong!")
        pure ()
      _ -> pure ()

isTextChannel :: Channel -> Bool
isTextChannel (ChannelText {}) = True
isTextChannel _ = False

fromBot :: Message -> Bool
fromBot m = userIsBot (messageAuthor m)

isPing :: Message -> Bool
isPing = ("ping" `T.isPrefixOf`) . T.map toLower . messageText
