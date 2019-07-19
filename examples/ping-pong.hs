{-# LANGUAGE OverloadedStrings #-}  -- allows "strings" to be Data.Text

import Control.Exception (finally)
import Control.Monad (when, forM_)
import Data.Char (toLower)
import Data.Monoid ((<>))
import Data.Default (def)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Discord

-- | Replies "pong" to every message that starts with "ping"
pingpongExample :: IO ()
pingpongExample = do
  tok <- T.strip <$> TIO.readFile "./examples/auth-token.secret"

  -- open ghci and run  [[ def :: RunDiscordOpts ]] to see default Opts
  runDiscord $ def { discordToken = tok
                   , discordOnStart = startHandler
                   , discordOnEvent = eventHandler
                   }
  pure ()

isTextChannel :: Channel -> Bool
isTextChannel (ChannelText {}) = True
isTextChannel _ = False

-- If a handler throws an exception, discord-haskell will gracefully shutdown
startHandler :: DiscordHandle -> IO ()
startHandler dis = do
  putStrLn "Starting the bot"

  Right partialGuilds <- restCall dis $ GetCurrentUserGuilds

  forM_ partialGuilds $ \pg -> do
    Right guild <- restCall dis $ GetGuild (partialGuildId pg)
    Right chans <- restCall dis $ GetGuildChannels (guildId guild)
    case filter isTextChannel chans of
      (c:_) -> do restCall dis $ CreateMessage (channelId c) "Hello! I will reply to pings with pongs"
                  pure ()
      _ -> pure ()

fromBot :: Message -> Bool
fromBot m = userIsBot (messageAuthor m)

isPing :: T.Text -> Bool
isPing = ("ping" `T.isPrefixOf`) . T.map toLower

eventHandler :: DiscordHandle -> Event -> IO ()
eventHandler dis event = case event of
      MessageCreate m -> when (not (fromBot m) && isPing (messageText m)) $ do
        resp <- restCall dis (CreateMessage (messageChannel m) "Pong!")
        putStrLn $ show resp <> "\n"
      _ -> pure ()

