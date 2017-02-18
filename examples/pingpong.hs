{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
import Data.Text
import Pipes

import Network.Discord

reply :: Message -> Text -> Effect DiscordM ()
reply Message{messageChannel=chan} cont = fetch' $ CreateMessage chan cont Nothing

main :: IO ()
main = runBot (Bot "TOKEN") $ do
  with ReadyEvent $ \(Init v u _ _ _) ->
    liftIO . putStrLn $ "Connected to gateway v" ++ show v ++ " as user " ++ show u

  with MessageCreateEvent $ \msg@Message{..} -> do
    when ("Ping" `isPrefixOf` messageContent && (not . userIsBot $ messageAuthor)) $
      reply msg "Pong!"
