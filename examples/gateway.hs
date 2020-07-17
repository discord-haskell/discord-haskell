{-# LANGUAGE OverloadedStrings #-}

import Data.Monoid ((<>))
import Control.Monad (void, forever)
import Control.Concurrent (forkIO, killThread)
import Control.Concurrent.Chan
import qualified Data.Text.IO as TIO

import Discord
import Discord.Types

-- | Prints every event as it happens
gatewayExample :: IO ()
gatewayExample = do
  tok <- TIO.readFile "./examples/auth-token.secret"

  outChan <- newChan :: IO (Chan String)

  -- Events are processed in new threads, but stdout isn't
  -- synchronized. We get ugly output when multiple threads
  -- write to stdout at the same time
  threadId <- forkIO $ forever $ readChan outChan >>= putStrLn

  void $ runDiscord $ def { discordToken = tok
                          , discordOnStart = startHandler
                          , discordOnEvent = eventHandler outChan
                          , discordOnEnd = killThread threadId
                          }

-- Events are enumerated in the discord docs
-- https://discord.com/developers/docs/topics/gateway#commands-and-events-gateway-events
eventHandler :: Chan String -> DiscordHandle -> Event -> IO ()
eventHandler out _dis event = writeChan out (show event <> "\n")


startHandler :: DiscordHandle -> IO ()
startHandler dis = do
  let opts = RequestGuildMembersOpts
        { requestGuildMembersOptsGuildId = 453207241294610442
        , requestGuildMembersOptsLimit = 100
        , requestGuildMembersOptsNamesStartingWith = ""
        }

  -- gateway commands are enumerated in the discord docs
  -- https://discord.com/developers/docs/topics/gateway#commands-and-events-gateway-commands
  sendCommand dis (RequestGuildMembers opts)


