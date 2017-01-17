{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
import Network.Discord
import Pipes
import Data.Text
import Language.Discord
import Control.Monad.IO.Class

main = runBot "TOKEN" $ do
  with ReadyEvent $ \_ -> do
    liftIO $ putStr "Hello, World!"
  with MessageCreateEvent $ \(MessageCreate (msg@Message{messageAuthor=User{userIsBot=bot}})) -> do
    liftIO $ print msg
    unless bot $
      fetch' $ CreateMessage "188134500411244545" . pack $ show msg
