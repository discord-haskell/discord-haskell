{-# LANGUAGE OverloadedStrings #-}

import Data.Char (isSpace)
import Data.Monoid ((<>))
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Discord

-- | Prints every event as it happens
cacheExample :: IO ()
cacheExample = do
  tok <- T.filter (not . isSpace) <$> TIO.readFile "./examples/auth-token.secret"
  dis <- loginRestGateway (Bot tok)
  cache <- readCache dis

  putStrLn ("Cached info from gateway: " <> show cache)

