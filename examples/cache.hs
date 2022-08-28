{-# LANGUAGE OverloadedStrings #-}

import UnliftIO (liftIO)

import Discord

import ExampleUtils (getToken)

main :: IO ()
main = cacheExample

-- There's not much information in the Cache for now
--   but this program will show you what its got

-- | Print cached Gateway info
cacheExample :: IO ()
cacheExample = do
  tok <- getToken

  _ <- runDiscord $ def { discordToken = tok
                        , discordOnStart = do
                               cache <- readCache
                               liftIO $ putStrLn ("Cached info from gateway: " <> show cache)
                               stopDiscord
                        }
  pure ()

