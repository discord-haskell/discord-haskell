{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Provides actions for Channel API interactions
module Discord.Rest.User
  ( UserRequest(..)
  ) where


import Data.Aeson
import Data.Monoid (mempty, (<>))
import Network.HTTP.Req ((/:))
import qualified Network.HTTP.Req as R

import Discord.Rest.Prelude
import Discord.Types

instance Request (UserRequest a) where
  majorRoute = userMajorRoute
  jsonRequest = userJsonRequest


-- | Data constructor for requests. See <https://discordapp.com/developers/docs/resources/ API>
data UserRequest a where
  -- | Returns the 'User' object of the requester's account. For OAuth2, this requires
  --   the identify scope, which will return the object without an email, and optionally
  --   the email scope, which returns the object with an email.
  GetCurrentUser       :: UserRequest User
  -- | Returns a 'User' for a given user ID
  GetUser              :: Snowflake -> UserRequest User
  -- | Modify the requestors user account settings. Returns a 'User' object on success.
  ModifyCurrentUser    :: ToJSON o => o -> UserRequest User
  -- | Returns a list of user 'Guild' objects the current user is a member of.
  --   Requires the guilds OAuth2 scope.
  GetCurrentUserGuilds :: UserRequest [PartialGuild]
  -- | Leave a guild.
  LeaveGuild           :: Snowflake -> UserRequest ()
  -- | Returns a list of DM 'Channel' objects
  GetUserDMs           :: UserRequest [Channel]
  -- | Create a new DM channel with a user. Returns a DM 'Channel' object.
  CreateDM             :: Snowflake -> UserRequest Channel


userMajorRoute :: UserRequest a -> String
userMajorRoute c = case c of
  (GetCurrentUser) ->                        "me "
  (GetUser _) ->                           "user "
  (ModifyCurrentUser _) ->          "modify_user "
  (GetCurrentUserGuilds) ->     "get_user_guilds "
  (LeaveGuild g) ->                 "leave_guild " <> show g
  (GetUserDMs) ->                       "get_dms "
  (CreateDM _) ->                       "make_dm "

-- | The base url (Req) for API requests
baseUrl :: R.Url 'R.Https
baseUrl = R.https "discordapp.com" R./: "api" R./: apiVersion
  where apiVersion = "v6"

users :: R.Url 'R.Https
users = baseUrl /: "users"

userJsonRequest :: UserRequest r -> JsonRequest
userJsonRequest c = case c of
  (GetCurrentUser) -> Get (users /: "@me") mempty

  (GetUser user) -> Get (users // user ) mempty

  (ModifyCurrentUser patch) ->
      Patch (users /: "@me")  (R.ReqBodyJson patch) mempty

  (GetCurrentUserGuilds) -> Get (users /: "@me" /: "guilds") mempty

  (LeaveGuild guild) -> Delete (users /: "@me" /: "guilds" // guild) mempty

  (GetUserDMs) -> Get (users /: "@me" /: "channels") mempty

  (CreateDM user) ->
      let body = R.ReqBodyJson $ object ["recipient_id" .= user]
      in Post (users /: "@me" /: "channels") (pure body) mempty
