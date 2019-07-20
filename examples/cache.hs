{-# LANGUAGE OverloadedStrings #-}

import Data.Monoid ((<>))
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Discord

--[[ Warning: temporaryily broken as I upgrade the interface ]]

-- | Print cached Gateway info
cacheExample :: IO ()
cacheExample = do
  tok <- T.strip <$> TIO.readFile "./examples/auth-token.secret"
  dis <- loginRestGateway (Auth tok)
  cache <- readCache dis

  putStrLn ("Cached info from gateway: " <> show cache)
  
  stopDiscord dis

