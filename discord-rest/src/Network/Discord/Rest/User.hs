{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Provide actions for User API interactions.
module Network.Discord.Rest.User
  (
    UserRequest(..)
  ) where

import Data.Aeson
import Data.Monoid (mempty, (<>))
import Network.HTTP.Req ((/:))
import qualified Network.HTTP.Req as R

import Network.Discord.Rest.Prelude
import Network.Discord.Types


instance DiscordRequest UserRequest where
  majorRoute :: UserRequest a -> String
  majorRoute = majorRouteUser

  createRequest :: FromJSON r => UserRequest r -> JsonRequest r
  createRequest = jsonRequestUser

-- | Data constructor for User requests. See
--   <https://discordapp.com/developers/docs/resources/user User API>
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
  GetCurrentUserGuilds :: Range -> UserRequest Guild
  -- | Leave a guild.
  LeaveGuild           :: Snowflake -> UserRequest ()
  -- | Returns a list of DM 'Channel' objects
  GetUserDMs           :: UserRequest [Channel]
  -- | Create a new DM channel with a user. Returns a DM 'Channel' object.
  CreateDM             :: Snowflake -> UserRequest Channel

majorRouteUser :: UserRequest a -> String
majorRouteUser c = case c of
  (GetCurrentUser) ->                      "me "
  (GetUser _) ->                         "user "
  (ModifyCurrentUser _) ->        "modify_user "
  (GetCurrentUserGuilds _) -> "get_user_guilds "
  (LeaveGuild g) ->               "leave_guild " <> show g
  (GetUserDMs) ->                     "get_dms "
  (CreateDM _) ->                     "make_dm "


url :: R.Url 'R.Https
url = baseUrl /: "users"

jsonRequestUser :: FromJSON r => UserRequest r -> JsonRequest r
jsonRequestUser c = case c of
      (GetCurrentUser) -> Get (url /: "@me") mempty

      (GetUser user) -> Get (url // user ) mempty

      (ModifyCurrentUser patch) ->
          Patch (url /: "@me")  (R.ReqBodyJson patch) mempty

      (GetCurrentUserGuilds range) -> Get url $ toQueryString range

      (LeaveGuild guild) -> Delete (url /: "@me" /: "guilds" // guild) mempty

      (GetUserDMs) -> Get (url /: "@me" /: "channels") mempty

      (CreateDM user) ->
          let body = R.ReqBodyJson $ object ["recipient_id" .= user]
          in Post (url /: "@me" /: "channels") (pure body) mempty
