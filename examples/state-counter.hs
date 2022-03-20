{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Control.Monad (when, void)
import Control.Exception (try, IOException)
import Control.Concurrent.Chan
import Control.Concurrent.MVar

import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Discord
import Discord.Types
import Discord.Handle (discordHandleLog)
import qualified Discord.Requests as R

data State = State { pingCount :: Integer }
  deriving (Show, Read, Eq, Ord)

-- | Counts how many pings we've seen across sessions
stateExample :: IO ()
stateExample = do
  tok <- TIO.readFile "./examples/auth-token.secret"

  -- try to read previous state, otherwise use 0
  state :: MVar (State) <- do
        mfile <- try $ read . T.unpack <$> TIO.readFile "./cachedState"
        s <- case mfile of
            Right file -> do
                    TIO.putStrLn "loaded state from file"
                    pure file
            Left (_ :: IOException) -> do
                    TIO.putStrLn "created new state"
                    pure $ State { pingCount = 0 }
        newMVar s

  TIO.putStrLn "starting ping loop"
  t <- runDiscord $ def { discordToken = tok
                        , discordOnEvent = eventHandler state
                        , discordOnLog = \s -> TIO.putStrLn s >> TIO.putStrLn ""

                        -- normal cleanup in discordOnEnd
                        , discordOnEnd = do s <- readMVar state
                                            TIO.writeFile "./cachedState" (T.pack (show s))
                        }
  TIO.putStrLn t


eventHandler :: MVar State -> DiscordHandle -> Event -> IO ()
eventHandler state handle event = case event of
  -- respond to message, and modify state
  MessageCreate m -> when (not (fromBot m) && isPing m) $ do
    writeChan (discordHandleLog handle) "got a ping!"

    s <- takeMVar state
    putMVar state $ State { pingCount = pingCount s + 1 }

    void $ restCall handle (R.CreateMessage (messageChannelId m) (T.pack ("Pong #" <> show (pingCount s))))

  _ -> pure ()


fromBot :: Message -> Bool
fromBot = userIsBot . messageAuthor

isPing :: Message -> Bool
isPing = ("ping" `T.isPrefixOf`) . T.toLower . messageContent
