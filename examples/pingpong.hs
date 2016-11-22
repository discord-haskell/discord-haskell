{-# LANGUAGE OverloadedStrings #-}
import Data.Text

import Network.Discord

data PingPongClient = PingPongClient
instance Client PingPongClient where
  getAuth _ = "TOKEN"

main :: IO ()
main = do
  gateway <- getGateway
  runWebsocket gateway PingPongClient $ do
    DiscordState {getWebSocket=ws} <- get
    (eventCore ~> \event -> case event of
      Ready (Init v u _ _ _) -> liftIO . putStrLn $ "Connected to gateway v"++show v
        ++ " as user " ++ show u
      MessageCreate (Message {messageChannel=chan, messageAuthor=user, messageContent=cont}) ->
        when ("Ping" `isPrefixOf` cont && not (userIsBot user)) $
          void $ restServer +>> fetch (CreateMessage chan "Pong!")
      _ -> return ()
      ) ws
