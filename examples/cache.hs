{-# LANGUAGE OverloadedStrings #-}

import Data.Monoid ((<>))
import qualified Data.Text.IO as TIO

import Discord

-- | Print cached Gateway info
cacheExample :: IO ()
cacheExample = do
  tok <- TIO.readFile "./examples/auth-token.secret"

  _ <- runDiscord $ def { discordToken = tok
                        , discordOnStart = \dis -> do cache <- readCache dis
                                                      putStrLn ("Cached info from gateway: " <> show cache)
                                                      stopDiscord dis
                        }
  pure ()

