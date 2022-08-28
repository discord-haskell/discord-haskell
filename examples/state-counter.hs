{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Control.Monad (when, void, forever)
import UnliftIO (try, IOException) -- liftIO
import UnliftIO.MVar
import UnliftIO.Chan
import UnliftIO.Concurrent (forkIO, killThread)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Discord
import Discord.Types
import qualified Discord.Requests as R

import ExampleUtils (getToken)

main :: IO ()
main = stateExample

data State = State { pingCount :: Integer }
  deriving (Show, Read, Eq, Ord)

-- | Counts how many pings we've seen across sessions
stateExample :: IO ()
stateExample = do
  tok <- getToken

  -- eventHandler is called concurrently, need to sync stdout
  printQueue <- newChan :: IO (Chan T.Text)
  threadId <- forkIO $ forever $ readChan printQueue >>= TIO.putStrLn

  -- try to read previous state, otherwise use 0
  state :: MVar (State) <- do
        mfile <- try $ read . T.unpack <$> TIO.readFile "./cachedState"
        s <- case mfile of
            Right file -> do
                    writeChan printQueue "loaded state from file"
                    pure file
            Left (_ :: IOException) -> do
                    writeChan printQueue "created new state"
                    pure $ State { pingCount = 0 }
        newMVar s

  t <- runDiscord $ def { discordToken = tok
                        , discordOnStart = writeChan printQueue "starting ping loop"
                        , discordOnEvent = eventHandler state printQueue
                        , discordOnEnd = do killThread threadId
                                            --
                                            s <- readMVar state
                                            TIO.writeFile "./cachedState" (T.pack (show s))
                        }
  TIO.putStrLn t


eventHandler :: MVar State -> Chan T.Text -> Event -> DiscordHandler ()
eventHandler state printQueue event = case event of
  -- respond to message, and modify state
  MessageCreate m -> when (not (fromBot m) && isPing m) $ do
    writeChan printQueue "got a ping!"

    s <- takeMVar state

    void $ restCall (R.CreateMessage (messageChannelId m) (T.pack ("Pong #" <> show (pingCount s))))

    putMVar state $ State { pingCount = pingCount s + 1 }

  _ -> pure ()


fromBot :: Message -> Bool
fromBot = userIsBot . messageAuthor

isPing :: Message -> Bool
isPing = ("ping" `T.isPrefixOf`) . T.toLower . messageContent
