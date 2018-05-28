{-# LANGUAGE ExistentialQuantification, TypeSynonymInstances, GeneralizedNewtypeDeriving #-}
-- | Provides base types and utility functions needed for modules in Network.Discord.Types
module Network.Discord.Types.Prelude where
  import Data.Bits
  import Data.Word

  import Data.Aeson.Types
  import Data.Hashable
  import Data.Text
  import Data.Time.Clock
  import Data.Time.Clock.POSIX
  import Control.Monad (mzero)

  -- | Authorization token for the Discord API
  data Auth = Bot    String
            | Client String
            | Bearer String

  -- | Formats the token for use with the REST API
  instance Show Auth where
    show (Bot    token) = "Bot "    ++ token
    show (Client token) = token
    show (Bearer token) = "Bearer " ++ token

  -- | Get the raw token formatted for use with the websocket gateway
  authToken :: Auth -> String
  authToken (Bot    token) = token
  authToken (Client token) = token
  authToken (Bearer token) = token

  -- | A unique integer identifier. Can be used to calculate the creation date of an entity.
  newtype Snowflake = Snowflake Word64
    deriving (Ord, Eq, Num, Integral, Enum, Real, Bits, Hashable)

  instance Show Snowflake where
    show (Snowflake a) = show a

  instance ToJSON Snowflake where
    toJSON (Snowflake snowflake) = String . pack $ show snowflake

  instance FromJSON Snowflake where
    parseJSON (String snowflake) = Snowflake <$> (return . read $ unpack snowflake)
    parseJSON _ = mzero

  -- |Gets a creation date from a snowflake.
  creationDate :: Snowflake -> UTCTime
  creationDate x = posixSecondsToUTCTime . realToFrac
    $ 1420070400 + quot (shiftR x 22) 1000

  -- | Default timestamp
  epochTime :: UTCTime
  epochTime = posixSecondsToUTCTime 0

  -- | Return only the Right vaule from an either
  justRight :: (Show a) => Either a b -> b
  justRight (Right b) = b
  justRight (Left a) = error $ show a

  -- | Convert ToJSON values to FromJSON values
  reparse :: (ToJSON a, FromJSON b) => a -> Either String b
  reparse val = parseEither parseJSON $ toJSON val
