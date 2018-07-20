{-# LANGUAGE OverloadedStrings #-}

import Control.Exception (finally)
import Control.Monad (forever, when)
import Data.Char (isSpace, toLower)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Discord

-- | Replies "pong" to every message that starts with "ping"
pingpongExample :: IO ()
pingpongExample = do
  tok <- T.filter (not . isSpace) <$> TIO.readFile "./examples/auth-token.secret"

  dis <- loginRestGateway (Bot tok)

  finally (forever $ do
              e <- nextEvent dis
              case e of
                  MessageCreate m -> when (isPing (messageContent m)) $ do
                    resp <- restCall dis (CreateMessage (messageChannel m) "Pong!" Nothing)
                    putStrLn (show resp)
                    putStrLn ""
                  _ -> pure ())
          (stopDiscord dis)

isPing :: T.Text -> Bool
isPing = T.isPrefixOf "ping" . T.map toLower
