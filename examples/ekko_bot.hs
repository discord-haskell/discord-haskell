{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

import GHC.Generics
import Control.Monad

import Network.Discord
import Network.Discord.Client
import Network.Discord.Http
import Network.Discord.Types
import Network.Discord.Gateway
import Data.Aeson
import Debug.Trace
data Application =
  Application {
      description :: String
    , icon        :: Maybe String
    , id          :: String
    , name        :: String
    , owner       :: User
    } deriving (Show, Generic)
instance FromJSON Application

getId :: Application -> Snowflake
getId (Application _ _ id _ _) = id

data EkkoBot = EkkoBot String
instance Client EkkoBot where
  getAuth _ = "Get your own damn secret"
  onReady client _ _ = do
    app <- apiGet client "/oauth2/applications/@me" :: IO (Either String Application)
    case app of
      Left s -> do
        putStrLn s
        return $ EkkoBot undefined
      Right o -> return $ EkkoBot $ getId o

  onMessageCreate (EkkoBot id) queue message = case decode $ encode message of
    Just (Message id ch ath cont time eTime tts mEvry ment mRoles att emb nonce pin) -> do
      if id == getUserId ath then putStrLn "User was a bot" else do
        message <- createMessage (EkkoBot id) ch cont
        print message
      return (EkkoBot id)
    _ -> return (EkkoBot id)

main :: IO ()
main = startClient $ EkkoBot undefined

getUserId :: User -> Snowflake
getUserId (User id _ _ _ _ _ _ _) = id
