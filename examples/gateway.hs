{-# LANGUAGE OverloadedStrings #-}

import Control.Monad (forever)
import Data.Char (isSpace)
import Data.Monoid ((<>))
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Discord

-- |Prints every event as it happens
a :: IO ()
a = do
  tok <- T.filter (not . isSpace) <$> TIO.readFile "./examples/auth-token.secret"
  dis <- login (Bot tok)
  forever $ do
    x <- nextEvent dis
    putStrLn (show x <> "\n")

