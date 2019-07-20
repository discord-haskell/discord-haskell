{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

import Data.Aeson
import Data.Monoid (mempty, (<>))
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Network.HTTP.Req as R

import Discord.Rest.Prelude
import Discord.Types

import Discord

--[[ Warning: temporaryily broken as I upgrade the interface ]]

-- | Create a different ModifyGuild that can only change the Name
--     If a rest call you need is missing it is possible to add it
customRestExample :: IO ()
customRestExample = do
  tok <- T.strip <$> TIO.readFile "./examples/auth-token.secret"
  dis <- loginRest (Auth tok)

  chan <- restCall dis (CustomModifyGuild 453207241294610442
                            (CustomModifyGuildOpts (Just "testing server")))
  putStrLn ("Channel object: " <> show chan <> "\n")


data CustomRequest a where
  CustomModifyGuild :: Snowflake -> CustomModifyGuildOpts -> CustomRequest Guild

data CustomModifyGuildOpts = CustomModifyGuildOpts
  { customModifyGuildOptsName :: Maybe T.Text} deriving (Show, Eq, Ord)

instance ToJSON CustomModifyGuildOpts where
  toJSON CustomModifyGuildOpts{..} =  object [(name, val) | (name, Just val) <-
                                  [("name", toJSON <$> customModifyGuildOptsName )]]

instance Request (CustomRequest a) where
  majorRoute = customMajorRoute
  jsonRequest = customJsonRequest

customMajorRoute :: CustomRequest a -> String
customMajorRoute c = case c of
  (CustomModifyGuild g _) ->                    "guild " <> show g

customJsonRequest :: CustomRequest r -> JsonRequest
customJsonRequest c = case c of
  (CustomModifyGuild guild patch) ->
      Patch (baseUrl // guild) (R.ReqBodyJson patch) mempty

-- | The base url (Req) for API requests
baseUrl :: R.Url 'R.Https
baseUrl = R.https "discordapp.com" R./: "api" R./: "v6" R./: "guilds"
