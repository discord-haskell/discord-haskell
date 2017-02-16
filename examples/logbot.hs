{-# LANGUAGE OverloadedStrings #-}
import Control.Monad.State
import Data.Text

import Pipes ((~>))
import Pipes.Core hiding (Client)

import Network.Discord.Types
import Network.Discord.Rest
import Network.Discord.Gateway

data LogClient = LogClient
instance Client LogClient where
  getAuth _ = Bot "TOKEN"

main :: IO ()
main = do
  gateway <- getGateway
  runWebsocket gateway LogClient $ do
    DiscordState {getWebSocket=ws} <- get
    (eventCore ~> \event -> case event of
      Ready (Init v u _ _ _) -> liftIO . putStrLn $ "Connected to gateway v"++show v
        ++ " as user " ++ show u
      MessageCreate msg@(Message {messageAuthor = User{userIsBot = bot}}) ->
        unless bot $
          fetch'
            $ CreateMessage 188134500411244545 (pack $ show msg) Nothing
      ev -> fetch'
        $ CreateMessage 188134500411244545 (pack $ show ev) Nothing
      ) ws
