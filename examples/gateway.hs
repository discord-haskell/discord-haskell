{-# LANGUAGE OverloadedStrings #-}

import Control.Monad (forever)
import Data.Char (isSpace)
import Data.Monoid ((<>))
import qualified Data.ByteString.Char8 as Q

import Discord

a :: IO ()
a = do
  tok <- Q.filter (not . isSpace) <$> Q.readFile "./examples/auth-token.secret"
  nextEvent <- loginGateway (Bot tok)
  forever $ do
    x <- nextEvent
    putStrLn (show x <> "\n")
    pure ()

