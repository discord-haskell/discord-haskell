{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Provides actions for Voice API interactions
module Discord.Internal.Rest.Voice
  ( VoiceRequest(..)
  ) where


import qualified Data.Text as T
import Discord.Internal.Rest.Prelude
import Discord.Internal.Types

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

voices :: T.Text
voices = baseUrl /: "voice"

voiceJsonRequest :: VoiceRequest r -> JsonRequest
voiceJsonRequest c = case c of
  (ListVoiceRegions) -> Get (voices /: "regions") mempty
