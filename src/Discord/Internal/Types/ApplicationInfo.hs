{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Data structures pertaining to Discord User
module Discord.Internal.Types.ApplicationInfo where

import Data.Aeson
import qualified Data.Text as T
import Discord.Internal.Types.Prelude

-- | Structure containing partial information about an Application
data FullApplication = FullApplication
  { fullApplicationID :: ApplicationId
  , fullApplicationName :: T.Text
  , fullApplicationFlags :: Int
  } deriving (Show, Eq, Read)

instance FromJSON FullApplication where
  parseJSON = withObject "FullApplication" $ \o ->
    FullApplication <$> o .: "id"
                    <*> o .: "name"
                    <*> o .: "flags"

