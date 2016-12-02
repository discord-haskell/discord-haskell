{-# LANGUAGE OverloadedStrings #-}
import Language.Discord

main = runBot "TOKEN" $
  on MessageCreate $
    doCommand "`" $
          command "userInfo" $ do
            user <- author
            send $ show user
      <|> command "ping" $ send "Pong"
      <|> command "channelInfo" $ do
            chanId <- channel
            chanInfo <- fetch (GetChannel chanId)
            send $ show chanInfo
