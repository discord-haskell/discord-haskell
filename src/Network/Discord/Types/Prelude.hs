{-# LANGUAGE ExistentialQuantification #-}
module Network.Discord.Types.Prelude where

  type Auth = String
  type Snowflake = String

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
