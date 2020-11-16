{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Provides actions for Channel API interactions
module Discord.Internal.Rest.Invite
  ( InviteRequest(..)
  ) where

import Network.HTTP.Req ((/:))
import qualified Network.HTTP.Req as R
import qualified Data.Text as T

import Discord.Internal.Rest.Prelude
import Discord.Internal.Types

instance Request (InviteRequest a) where
  majorRoute = inviteMajorRoute
  jsonRequest = inviteJsonRequest


-- | Data constructor for requests. See <https://discord.com/developers/docs/resources/ API>
data InviteRequest a where
  -- | Get invite for given code
  GetInvite :: T.Text -> InviteRequest Invite
  -- | Delete invite by code
  DeleteInvite :: T.Text -> InviteRequest Invite

inviteMajorRoute :: InviteRequest a -> String
inviteMajorRoute c = case c of
  (GetInvite _) ->     "invite "
  (DeleteInvite _) ->  "invite "

invite :: R.Url 'R.Https
invite = baseUrl /: "invites"

inviteJsonRequest :: InviteRequest r -> JsonRequest
inviteJsonRequest c = case c of
  (GetInvite g) -> Get (invite R./: g) mempty
  (DeleteInvite g) -> Delete (invite R./: g) mempty
