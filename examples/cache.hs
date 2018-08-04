{-# LANGUAGE OverloadedStrings #-}

import Data.Char (isSpace)
import Data.Monoid ((<>))
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Discord

-- | Print cached Gateway info
cacheExample :: IO ()
cacheExample = do
  tok <- T.filter (not . isSpace) <$> TIO.readFile "./examples/auth-token.secret"
  dis <- loginRestGateway (Auth tok)
  cache <- readCache dis

  putStrLn ("Cached info from gateway: " <> show cache)

