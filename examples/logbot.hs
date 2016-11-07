{-# LANGUAGE OverloadedStrings #-}
import Control.Monad.State
import Data.Text

import Pipes ((~>))
import Pipes.Core hiding (Client)
import System.Remote.Monitoring

import Network.Discord.Types
import Network.Discord.Rest
import Network.Discord.Gateway

data LogClient = LogClient
instance Client LogClient where
  getAuth _ = "TOKEN"

main :: IO ()
main = do
  forkServer "localhost" 8080
  gateway <- getGateway
  runWebsocket gateway LogClient $ do
    DiscordState {getWebSocket=ws} <- get
    (eventCore ~> \event -> case event of
      Ready (Init v u _ _ _) -> liftIO . putStrLn $ "Connected to gateway v"++show v
        ++ " as user " ++ show u
      MessageCreate msg@(Message _ _ (User _ _ _ _ bot _ _ _) _ _ _ _ _ _ _ _ _ _ _) ->
        unless bot $
          void (restServer +>> fetch $ CreateMessage "188134500411244545" . pack $ show msg)
      ev -> void (restServer +>> fetch $ CreateMessage "188134500411244545" . pack $ show ev)
      ) ws
