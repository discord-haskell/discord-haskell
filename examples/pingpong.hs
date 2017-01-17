{-# LANGUAGE OverloadedStrings #-}
import Data.Text
import Pipes

import Network.Discord
import Language.Discord

reply :: Message -> Text -> Effect DiscordM ()
reply Message{messageChannel=chan} cont = fetch' $ CreateMessage chan cont

main :: IO ()
main = runBot "TOKEN" $ do
  with ReadyEvent $ \_ ->
    liftIO $ putStrLn "Online"

  with MessageCreateEvent $ \(MessageCreate msg) -> do
    when (messageContent msg == "Ping!") $
      reply msg "Pong!"
