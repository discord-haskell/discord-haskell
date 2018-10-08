{-# LANGUAGE OverloadedStrings #-}

import Control.Exception (finally)
import Control.Monad (forever, when)
import Data.Char (toLower)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Discord

-- | Replies "pong" to every message that starts with "ping"
pingpongExample :: IO ()
pingpongExample = do
  tok <- T.strip <$> TIO.readFile "./examples/auth-token.secret"

  dis <- loginRestGateway (Auth tok)

  finally (forever $ do
              e <- nextEvent dis
              case e of
                  MessageCreate m -> when (isPing (messageText m)) $ do
                    resp <- restCall dis (CreateMessage (messageChannel m) "Pong!" Nothing)
                    putStrLn (show resp)
                    putStrLn ""
                  _ -> pure ())
          (stopDiscord dis)

isPing :: T.Text -> Bool
isPing = T.isPrefixOf "ping" . T.map toLower
