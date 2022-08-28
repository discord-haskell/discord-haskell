{-# LANGUAGE OverloadedStrings #-}

import Control.Monad (forever)
import Control.Concurrent (forkIO, killThread)
import UnliftIO (liftIO)
import Control.Concurrent.Chan
import qualified Data.Text.IO as TIO

import Discord
import Discord.Types

import ExampleUtils (getToken, getGuildId)

main :: IO ()
main = gatewayExample

-- | Prints every event as it happens
gatewayExample :: IO ()
gatewayExample = do
  tok <- getToken
  testserverid <- getGuildId

  outChan <- newChan :: IO (Chan String)

  -- Events are processed in new threads, but stdout isn't
  -- synchronized. We get ugly output when multiple threads
  -- write to stdout at the same time
  threadId <- forkIO $ forever $ readChan outChan >>= putStrLn

  err <- runDiscord $ def { discordToken = tok
                          , discordOnStart = startHandler testserverid
                          , discordOnEvent = eventHandler outChan
                          , discordOnEnd = killThread threadId
                          }
  TIO.putStrLn err

-- Events are enumerated in the discord docs
-- https://discord.com/developers/docs/topics/gateway#commands-and-events-gateway-events
eventHandler :: Chan String -> Event -> DiscordHandler ()
eventHandler out event = liftIO $ writeChan out (show event <> "\n")


startHandler :: GuildId -> DiscordHandler ()
startHandler testserverid = do
  let opts = RequestGuildMembersOpts
        { requestGuildMembersOptsGuildId = testserverid
        , requestGuildMembersOptsLimit = 100
        , requestGuildMembersOptsNamesStartingWith = ""
        }

  -- gateway commands are enumerated in the discord docs
  -- https://discord.com/developers/docs/topics/gateway#commands-and-events-gateway-commands
  sendCommand (RequestGuildMembers opts)
