{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FlexibleInstances  #-}

-- | Provides base types and utility functions needed for modules in Discord.Internal.Types
module Discord.Internal.Types.Prelude where

import Data.Bits
import Data.Word

import Data.Aeson.Types
import Data.Time.Clock
import qualified Data.Text as T
import Data.Time.Clock.POSIX
import Control.Monad (mzero)

import Data.Functor.Compose (Compose(Compose, getCompose))
import Data.Bifunctor (first)
import Data.Data (Data (dataTypeOf), dataTypeConstrs, fromConstr)
import Data.Maybe (fromJust)

-- | Authorization token for the Discord API
data Auth = Auth T.Text
  deriving (Show, Read, Eq, Ord)


-- | Get the raw token formatted for use with the websocket gateway
authToken :: Auth -> T.Text
authToken (Auth tok) = let token = T.strip tok
                           bot = if "Bot " `T.isPrefixOf` token then "" else "Bot "
                       in bot <> token

-- | A unique integer identifier. Can be used to calculate the creation date of an entity.
newtype Snowflake = Snowflake Word64
  deriving (Ord, Eq, Num, Integral, Enum, Real, Bits)

instance Show Snowflake where
  show (Snowflake a) = show a

instance Read Snowflake where
  readsPrec p = getCompose $ first Snowflake <$> Compose (readsPrec p)

instance ToJSON Snowflake where
  toJSON (Snowflake snowflake) = String . T.pack $ show snowflake

instance FromJSON Snowflake where
  parseJSON (String snowflake) = pure . read $ T.unpack snowflake
  parseJSON _ = mzero

type ChannelId = Snowflake
type StageId = Snowflake
type GuildId = Snowflake
type MessageId = Snowflake
type AttachmentId = Snowflake
type EmojiId = Snowflake
type StickerId = Snowflake
type UserId = Snowflake
type OverwriteId = Snowflake
type RoleId = Snowflake
type IntegrationId = Snowflake
type WebhookId = Snowflake
type ParentId = Snowflake
type ApplicationId = Snowflake 
type ApplicationCommandId = Snowflake
type InteractionId = Snowflake
type InteractionToken = T.Text
type Shard = (Int, Int)

-- | Gets a creation date from a snowflake.
snowflakeCreationDate :: Snowflake -> UTCTime
snowflakeCreationDate x = posixSecondsToUTCTime . realToFrac
  $ 1420070400 + quot (shiftR x 22) 1000

-- | Default timestamp
epochTime :: UTCTime
epochTime = posixSecondsToUTCTime 0

type ColorInteger = Integer

makeTable :: (Data t, Enum t) => t -> [(Int, t)]
makeTable t = map (\cData -> let c = fromConstr cData in (fromEnum c, c)) (dataTypeConstrs $ dataTypeOf t)

toMaybeJSON :: (ToJSON a) => a -> Maybe Value
toMaybeJSON = return . toJSON

-- | What type of interaction has a user requested? Each requires its own type
-- of response.
data InteractionType
  = InteractionTypePing
  | InteractionTypeApplicationCommand
  | InteractionTypeMessageComponent
  | InteractionTypeApplicationCommandAutocomplete
  deriving (Show, Read, Data, Eq, Ord)

instance Enum InteractionType where
  fromEnum InteractionTypePing = 1
  fromEnum InteractionTypeApplicationCommand = 2
  fromEnum InteractionTypeMessageComponent = 3
  fromEnum InteractionTypeApplicationCommandAutocomplete = 4
  toEnum a = fromJust $ lookup a table
    where
      table = makeTable InteractionTypePing

instance ToJSON InteractionType where
  toJSON = toJSON . fromEnum

instance FromJSON InteractionType where
  parseJSON = withScientific "InteractionType" (return . toEnum . round)

class Internals a b where
  toInternal :: a -> b
  fromInternal :: b -> Maybe a

instance Internals a a where
  toInternal = id
  fromInternal = Just
