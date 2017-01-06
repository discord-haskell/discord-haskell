{-# LANGUAGE ExistentialQuantification #-}
module Network.Discord.Types.Prelude where
  import Data.Aeson.Types
  import Data.Time.Clock
  import Data.Time.Clock.POSIX
  import Data.Bits

  type Auth = String
  -- |A unique integer identifier. Can be used to calculate the creation date of an entity.
  type Snowflake = String

  -- |Gets a creation date from a snowflake.
  creationDate :: Snowflake -> UTCTime
  creationDate x = posixSecondsToUTCTime $ realToFrac(1420070400 + (((read x :: Int) `shiftR` 22) `quot` 1000))

  delete :: Eq a => a -> [(a, b)] -> [(a, b)]
  delete k ((x,y):xs)
    | k == x = delete k xs
    | otherwise = (x, y):delete k xs
  delete _ [] = []

  insert :: Eq a => a -> b -> [(a, b)] -> [(a, b)]
  insert k v s = (k, v):delete k s

  justRight :: (Show a) => Either a b -> b
  justRight (Right b) = b
  justRight (Left a) = error $ show a

  reparse :: (ToJSON a, FromJSON b) => a -> b
  reparse val = justRight . parseEither parseJSON $ toJSON val
