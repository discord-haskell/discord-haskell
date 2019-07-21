{-# LANGUAGE OverloadedStrings #-}  -- allows "strings" to be Data.Text

import Control.Monad (when, forM_)
import Control.Concurrent (threadDelay)
import Data.Char (toLower)
import Data.Default (def)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Discord
import Discord.Types
import qualified Discord.Requests as R

main :: IO ()
main = pingpongExample

-- | Replies "pong" to every message that starts with "ping"
pingpongExample :: IO ()
pingpongExample = do
  tok <- TIO.readFile "./examples/auth-token.secret"

  -- open ghci and run  [[ def :: RunDiscordOpts ]] to see default Opts
  t <- runDiscord $ def { discordToken = tok
                        , discordOnStart = startHandler
                        , discordOnEnd = putStrLn "Ended"
                        , discordOnEvent = eventHandler
                        , discordOnLog = \s -> TIO.putStrLn s >> TIO.putStrLn ""
                        }
  threadDelay (1 `div` 10 * 10^6)
  TIO.putStrLn t

-- If a handler throws an exception, discord-haskell will gracefully shutdown
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

eventHandler :: DiscordHandle -> Event -> IO ()
eventHandler dis event = case event of
      MessageCreate m -> when (not (fromBot m) && isPing (messageText m)) $ do
        threadDelay (4 * 10^6)
        _ <- restCall dis (R.CreateMessage (messageChannel m) "Pong!")
        pure ()
      _ -> pure ()

isTextChannel :: Channel -> Bool
isTextChannel (ChannelText {}) = True
isTextChannel _ = False

fromBot :: Message -> Bool
fromBot m = userIsBot (messageAuthor m)

isPing :: T.Text -> Bool
isPing = ("ping" `T.isPrefixOf`) . T.map toLower
