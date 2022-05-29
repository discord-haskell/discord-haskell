{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Provides actions for Voice API interactions
module Discord.Internal.Rest.Voice
  ( VoiceRequest (..),
  )
where

import Discord.Internal.Rest.Prelude
import Discord.Internal.Types
import Network.HTTP.Req ((/:))
import qualified Network.HTTP.Req as R

instance Request (VoiceRequest a) where
  majorRoute = voiceMajorRoute
  jsonRequest = voiceJsonRequest

-- | Data constructor for requests
data VoiceRequest a where
  -- | List all available 'VoiceRegion's.
  ListVoiceRegions :: VoiceRequest [VoiceRegion]

voiceMajorRoute :: VoiceRequest a -> String
voiceMajorRoute c = case c of
  (ListVoiceRegions) -> "whatever "

voices :: R.Url 'R.Https
voices = baseUrl /: "voice"

voiceJsonRequest :: VoiceRequest r -> JsonRequest
voiceJsonRequest c = case c of
  (ListVoiceRegions) -> Get (voices /: "regions") mempty
