{-# LANGUAGE ExistentialQuantification, MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings, FlexibleInstances #-}
module Network.Discord.Rest.Prelude where
  import Data.ByteString (append)
  import Data.ByteString.Char8 (pack)
  import Data.Time.Clock.POSIX
  import Control.Concurrent (threadDelay)

  import Data.Aeson
  import Network.Wreq
  import Control.Lens
  import Data.Hashable
  import qualified Control.Monad.State as St
  import Paths_discord_hs (version)
  import Data.Version     (showVersion)

  import Network.Discord.Types

  readInteger :: String -> Integer
  readInteger = read

  baseURL :: String
  baseURL = "https://discordapp.com/api/v6"

  baseRequest :: DiscordM Options
  baseRequest = do
    DiscordState {getClient=client} <- St.get
    return $ defaults
      & header "Authorization" .~ [append "Bot " . pack $ getAuth client]
      & header "User-Agent"    .~
        [pack  $ "DiscordBot (https://github.com/jano017/Discord.hs,"
          ++ showVersion version
          ++ ")"]
      & header "Content-Type" .~ ["application/json"]

  class RateLimit a where
    getRateLimit  :: a -> DiscordM (Maybe Int)
    setRateLimit  :: a -> Int -> DiscordM ()
    waitRateLimit :: a -> DiscordM ()
    waitRateLimit endpoint = do
      rl <- getRateLimit endpoint
      case rl of
        Nothing -> return ()
        Just a  -> do
          now <- St.liftIO (fmap round getPOSIXTime :: IO Int)
          St.liftIO $ do
            putStrLn "Waiting for rate limit to reset..."
            threadDelay $ 1000000 * (a - now)
            putStrLn "Done"
          return ()

  class DoFetch a where
    doFetch :: a -> DiscordM Fetched

  data Fetchable = forall a. (DoFetch a, Hashable a) => Fetch a

  instance DoFetch Fetchable where
    doFetch (Fetch a) = doFetch a

  instance Hashable Fetchable where
    hashWithSalt s (Fetch a) = hashWithSalt s a

  instance Eq Fetchable where
    (Fetch a) == (Fetch b) = hash a == hash b

  data Fetched = forall a. (FromJSON a) => SyncFetched a
