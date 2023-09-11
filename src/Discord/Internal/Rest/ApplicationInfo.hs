{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Provides actions for Channel API interactions
module Discord.Internal.Rest.ApplicationInfo
  ( FullApplicationRequest(..)
  ) where

import Network.HTTP.Req ((/:))
import qualified Network.HTTP.Req as R

import Discord.Internal.Rest.Prelude
import Discord.Internal.Types

instance Request (FullApplicationRequest a) where
  majorRoute =  applicationMajorRoute
  jsonRequest = applicationJsonRequest


-- | Data constructor for requests. See <https://discord.com/developers/docs/resources/ API>
data FullApplicationRequest a where
  -- | Get the full application for the current user
  GetCurrentApplication :: FullApplicationRequest FullApplication

applicationMajorRoute :: FullApplicationRequest a -> String
applicationMajorRoute c = case c of
  (GetCurrentApplication) ->     "application "

applications :: R.Url 'R.Https
applications = baseUrl /: "applications"

applicationJsonRequest :: FullApplicationRequest r -> JsonRequest
applicationJsonRequest c = case c of
  (GetCurrentApplication) -> Get (applications /: "@me") mempty
