{-# OPTIONS_HADDOCK prune, not-home #-}
-- | Provides types and encoding/decoding code. Types should be identical to those provided
--   in the Discord API documentation.
module Network.Discord.Types
  ( module Network.Discord.Types
  , module Network.Discord.Types.Prelude
  , module Network.Discord.Types.Channel
  , module Network.Discord.Types.Events
  , module Network.Discord.Types.Gateway
  , module Network.Discord.Types.Guild
  , module Data.Aeson
  ) where

import Network.Discord.Types.Channel
import Network.Discord.Types.Events
import Network.Discord.Types.Gateway
import Network.Discord.Types.Guild
import Network.Discord.Types.Prelude

import Data.Aeson (Object)
import qualified Data.ByteString.Char8 as Q

data DiscordAuth = DiscordAuth {
    authAuth :: Auth
  , authVersion :: Q.ByteString
  }

