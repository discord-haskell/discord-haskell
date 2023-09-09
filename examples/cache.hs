{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

import UnliftIO (liftIO)

import Discord
import Discord.Types

import qualified Data.Text as T

import ExampleUtils (getToken)

main :: IO ()
main = cacheExample

-- There's not much information in the Cache for now
--   but this program will show you what its got

-- Note that the cache has to be enabled, and that this program will only print 
-- out when a message is received.

-- | Print cached Gateway info
cacheExample :: IO ()
cacheExample = do
  tok <- getToken

  t <- runDiscord $ def { discordToken = tok
                        , discordOnLog = putStrLn . T.unpack
                        , discordOnEvent = \case
                            MessageCreate _ -> readCache >>= liftIO . putStrLn . ("Current info as of new message: " <>) . show
                            _ -> pure ()
                        , discordEnableCache = True
                        }
  print t

