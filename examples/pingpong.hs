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
      MessageCreate (Message _ chan (User _ _ _ _ bot _ _ _) cont _ _ _ _ _ _ _ _ _ _) ->
        when ("Ping" `isPrefixOf` cont && not bot) $
          void $ restServer +>> fetch (CreateMessage chan "Pong!")
      _ -> return ()
      ) ws
