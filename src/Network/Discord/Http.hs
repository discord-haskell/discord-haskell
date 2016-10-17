{-# LANGUAGE OverloadedStrings #-}
module Network.Discord.Http where
  import Data.ByteString.Char8
  import System.IO.Unsafe
  import Control.Monad (mzero)
  import Paths_discord_hs (version)
  import Data.Version (showVersion)

  import Network.HTTP.Conduit
  import Network.HTTP.Types.Header
  import Data.Aeson

  import Network.Discord.Client
  import Network.Discord.Types

  baseURL :: String
  baseURL = "/api"

  baseRequest :: Request
  baseRequest = defaultRequest {secure=True
                               , port=443, host="discordapp.com"
                               , requestHeaders = [
                                   (hUserAgent, pack $ "DiscordBot(https://github.com/jano017/Discord.hs, " ++ showVersion version ++ ")")
                                 , (hContentType, "application/json")
                                 ]
                               }

  addRequestHeader :: Request -> Header -> Request
  addRequestHeader req header = req {requestHeaders = header:requestHeaders req}

  addAuth :: (Client a0) => Request -> a0 -> Request
  addAuth req client = addRequestHeader req (hAuthorization, pack $ "Bot " ++ getAuth client)

  discordManager :: Manager
  {-# NOINLINE discordManager #-}
  discordManager = unsafePerformIO $ newManager tlsManagerSettings

  apiPost :: (Client a, ToJSON a0, FromJSON a1) => a -> String -> a0 -> IO (Either String a1)
  apiPost client endpoint body = do
    res <- httpLbs (addAuth baseRequest { method="POST", path = pack $ baseURL ++ endpoint, requestBody = RequestBodyLBS $ encode body } client) discordManager
    return $ eitherDecode $ responseBody res

  apiGet :: (Client a, FromJSON a0) => a -> String -> IO (Either String a0)
  apiGet client endpoint = do
    res <- httpLbs (addAuth baseRequest { method="GET", path = pack $ baseURL ++ endpoint } client) discordManager
    return $ eitherDecode $ responseBody res

  apiPut :: (Client a, ToJSON a0, FromJSON a1) => a -> String -> a0 -> IO (Either String a1)
  apiPut client endpoint body = do
    res <- httpLbs (addAuth baseRequest { method="PUT", path = pack $ baseURL ++ endpoint, requestBody = RequestBodyLBS $ encode body } client) discordManager
    return $ eitherDecode $ responseBody res

  apiPatch :: (Client a, ToJSON a0, FromJSON a1) => a -> String -> a0 -> IO (Either String a1)
  apiPatch client endpoint body = do
    res <- httpLbs (addAuth baseRequest { method="PATCH", path = pack $ baseURL ++ endpoint, requestBody = RequestBodyLBS $ encode body } client) discordManager
    return $ eitherDecode $ responseBody res

  apiDelete :: (Client a, FromJSON a0) => a -> String -> IO (Either String a0)
  apiDelete client endpoint = do
    res <- httpLbs (addAuth baseRequest { method="DELETE", path = pack $ baseURL ++ endpoint } client) discordManager
    return $ eitherDecode $ responseBody res

  createMessage :: (Client a) => a -> Snowflake -> String -> IO (Either String Message)
  createMessage client chan content = apiPost client ("/channels/" ++ chan ++ "/messages") $ object ["content" .= content]

  data GatewayURL = Gateway String
  instance FromJSON GatewayURL where
    parseJSON (Object o) = Gateway <$> o .: "url"
    parseJSON _ = mzero

  getGateway :: (Client a) => a -> IO String
  getGateway client = do
    urlOrError <- apiGet client "/gateway"
    case urlOrError of
      Right (Gateway url) -> return url
      Left  (reason)      -> do
        Prelude.putStrLn reason
        return mzero
