{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Provides actions for Channel API interactions
module Discord.Internal.Rest.User
  ( UserRequest(..)
  , parseAvatarImage
  ) where


import Data.Aeson
import Network.HTTP.Req ((/:), (/~))
import qualified Network.HTTP.Req as R
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString as B
import qualified Data.ByteString.Base64 as B64

import Discord.Internal.Rest.Prelude
import Discord.Internal.Types

instance Request (UserRequest a) where
  majorRoute = userMajorRoute
  jsonRequest = userJsonRequest


-- | Data constructor for requests. See <https://discord.com/developers/docs/resources/ API>
data UserRequest a where
  -- | Returns the 'User' object of the requester's account. For OAuth2, this requires
  --   the identify scope, which will return the object without an email, and optionally
  --   the email scope, which returns the object with an email.
  GetCurrentUser       :: UserRequest User
  -- | Returns a 'User' for a given user ID
  GetUser              :: UserId -> UserRequest User
  -- | Modify user's username & avatar pic
  ModifyCurrentUser    :: T.Text -> Base64Image User -> UserRequest User
  -- | Returns a list of user 'Guild' objects the current user is a member of.
  --   Requires the guilds OAuth2 scope.
  GetCurrentUserGuilds :: UserRequest [PartialGuild]
  -- | Leave a guild.
  LeaveGuild           :: GuildId -> UserRequest ()
  -- | Returns a list of DM 'Channel' objects
  GetUserDMs           :: UserRequest [Channel]
  -- | Create a new DM channel with a user. Returns a DM 'Channel' object.
  CreateDM             :: UserId -> UserRequest Channel

  GetUserConnections   :: UserRequest [ConnectionObject]

-- | @parseAvatarImage bs@ will attempt to convert the given image bytestring
-- @bs@ to the base64 format expected by the Discord API. It may return Left
-- with an error reason if the image format could not be predetermined from the
-- opening magic bytes. This function does /not/ validate the rest of the image,
-- and this is up to the library user to check themselves.
--
-- This function accepts all file types accepted by 'getMimeType'.
parseAvatarImage :: B.ByteString -> Either T.Text (Base64Image User)
parseAvatarImage bs
  | Just mime <- getMimeType bs = Right (Base64Image mime (TE.decodeUtf8 (B64.encode bs)))
  | otherwise                   = Left "Unsupported image format provided"

userMajorRoute :: UserRequest a -> String
userMajorRoute c = case c of
  (GetCurrentUser) ->                        "me "
  (GetUser _) ->                           "user "
  (ModifyCurrentUser _ _) ->        "modify_user "
  (GetCurrentUserGuilds) ->     "get_user_guilds "
  (LeaveGuild g) ->                 "leave_guild " <> show g
  (GetUserDMs) ->                       "get_dms "
  (CreateDM _) ->                       "make_dm "
  (GetUserConnections) ->           "connections "

users :: R.Url 'R.Https
users = baseUrl /: "users"

userJsonRequest :: UserRequest r -> JsonRequest
userJsonRequest c = case c of
  (GetCurrentUser) -> Get (users /: "@me") mempty

  (GetUser user) -> Get (users /~ user ) mempty

  (ModifyCurrentUser name b64im) ->
      Patch (users /: "@me")  (pure (R.ReqBodyJson (object [ "username" .= name
                                                           , "avatar" .= b64im ]))) mempty

  (GetCurrentUserGuilds) -> Get (users /: "@me" /: "guilds") mempty

  (LeaveGuild guild) -> Delete (users /: "@me" /: "guilds" /~ guild) mempty

  (GetUserDMs) -> Get (users /: "@me" /: "channels") mempty

  (CreateDM user) ->
      let body = R.ReqBodyJson $ object ["recipient_id" .= user]
      in Post (users /: "@me" /: "channels") (pure body) mempty

  (GetUserConnections) ->
    Get (users /: "@me" /: "connections") mempty
