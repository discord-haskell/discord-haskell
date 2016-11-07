{-# LANGUAGE OverloadedStrings #-}
import Control.Monad.State
import Data.Text

import Pipes ((~>))
import Pipes.Core hiding (Client)
import System.Remote.Monitoring

import Network.Discord.Types
import Network.Discord.Rest
import Network.Discord.Gateway

data PingPongClient = PingPongClient
instance Client PingPongClient where
  getAuth _ = "TOKEN"

main :: IO ()
main = do
  forkServer "localhost" 8080
  gateway <- getGateway
  runWebsocket gateway PingPongClient $ do
    DiscordState {getWebSocket=ws} <- get
    (eventCore ~> \event -> case event of
      Ready (Init v u _ _ _) -> liftIO . putStrLn $ "Connected to gateway v"++show v
        ++ " as user " ++ show u
      MessageCreate (Message _ chan (User _ _ _ _ bot _ _ _) cont _ _ _ _ _ _ _ _ _ _) ->
        when ("Ping" `isPrefixOf` cont && not bot) $
          void (restServer +>> (request . Fetch $ CreateMessage chan "Pong!"))
      ev -> liftIO $ print ev
      ) ws
