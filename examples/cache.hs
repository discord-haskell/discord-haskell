{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text.IO as TIO
import Discord

-- There's not much information in the Cache for now
--   but this program will show you what its got

-- | Print cached Gateway info
cacheExample :: IO ()
cacheExample = do
  tok <- TIO.readFile "./examples/auth-token.secret"

  _ <- runDiscord $ def { discordToken = tok
                        , discordOnStart = \handle -> do
                               cache <- readCache handle
                               putStrLn ("Cached info from gateway: " <> show cache)
                               stopDiscord handle
                        }
  pure ()

