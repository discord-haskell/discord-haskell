{-# LANGUAGE DeriveDataTypeable #-}

-- | Data structures pertaining to Discord Colors
module Discord.Internal.Types.Color where


import Text.Read (readMaybe)
import Data.Maybe (fromMaybe)
import Data.Char (toLower)
import Data.Aeson
import Data.Data
import Control.Applicative (Alternative((<|>)))
import Data.Bits (Bits((.&.)))


import Discord.Internal.Types.Prelude (InternalDiscordEnum(..))

-- | Color names
-- Color is a bit of a mess on discord embeds.
-- I've here stolen the pallet list from https://gist.github.com/thomasbnt/b6f455e2c7d743b796917fa3c205f812
--
-- All discord embed color stuff is credited to
-- https://github.com/WarwickTabletop/tablebot/pull/34
data DiscordColor
  = -- | An RGB color with values in @[0..255]@ 
    DiscordColorRGB Integer Integer Integer
  | DiscordColorDefault
  | DiscordColorAqua
  | DiscordColorDarkAqua
  | DiscordColorGreen
  | DiscordColorDarkGreen
  | DiscordColorBlue
  | DiscordColorDarkBlue
  | DiscordColorPurple
  | DiscordColorDarkPurple
  | DiscordColorLuminousVividPink
  | DiscordColorDarkVividPink
  | DiscordColorGold
  | DiscordColorDarkGold
  | DiscordColorOrange
  | DiscordColorDarkOrange
  | DiscordColorRed
  | DiscordColorDarkRed
  | DiscordColorGray
  | DiscordColorDarkGray
  | DiscordColorDarkerGray
  | DiscordColorLightGray
  | DiscordColorNavy
  | DiscordColorDarkNavy
  | DiscordColorYellow
  | DiscordColorDiscordWhite
  | DiscordColorDiscordBlurple
  | DiscordColorDiscordGrayple
  | DiscordColorDiscordDarkButNotBlack
  | DiscordColorDiscordNotQuiteBlack
  | DiscordColorDiscordGreen
  | DiscordColorDiscordYellow
  | DiscordColorDiscordFuschia
  | DiscordColorDiscordRed
  | DiscordColorDiscordBlack
  deriving (Show, Read, Eq, Ord, Data)

-- | @hexToRGB@ attempts to convert a potential hex string into its decimal RGB
-- components.
hexToRGB :: String -> Maybe (Integer, Integer, Integer)
hexToRGB hex = do
  let h = map toLower hex
  r <- take2 h >>= toDec
  g <- drop2 h >>= take2 >>= toDec
  b <- drop2 h >>= drop2 >>= toDec
  return (r, g, b)
  where
    take2 (a:b:_) = Just [a, b]
    take2 _ = Nothing
    drop2 (_ : _ : as) = Just as
    drop2 _ = Nothing
    toDec :: String -> Maybe Integer
    toDec [s, u] = do
      a <- charToDec s
      b <- charToDec u
      return $ a * 16 + b
    toDec _ = Nothing
    charToDec :: Char -> Maybe Integer
    charToDec 'a' = Just 10
    charToDec 'b' = Just 11
    charToDec 'c' = Just 12
    charToDec 'd' = Just 13
    charToDec 'e' = Just 14
    charToDec 'f' = Just 15
    charToDec c = readMaybe [c]

-- | @hexToDiscordColor@ converts a potential hex string into a DiscordColor,
-- evaluating to Default if it fails.
hexToDiscordColor :: String -> DiscordColor
hexToDiscordColor hex =
  let (r, g, b) = fromMaybe (0, 0, 0) $ hexToRGB hex
   in DiscordColorRGB r g b

-- | Convert a color to its internal `Integer` representation
colorToInternal :: DiscordColor -> Integer
-- colorToInternal (DiscordColor i) = i
colorToInternal (DiscordColorRGB r g b) = (r * 256 + g) * 256 + b
colorToInternal DiscordColorDefault = 0
colorToInternal DiscordColorAqua = 1752220
colorToInternal DiscordColorDarkAqua = 1146986
colorToInternal DiscordColorGreen = 3066993
colorToInternal DiscordColorDarkGreen = 2067276
colorToInternal DiscordColorBlue = 3447003
colorToInternal DiscordColorDarkBlue = 2123412
colorToInternal DiscordColorPurple = 10181046
colorToInternal DiscordColorDarkPurple = 7419530
colorToInternal DiscordColorLuminousVividPink = 15277667
colorToInternal DiscordColorDarkVividPink = 11342935
colorToInternal DiscordColorGold = 15844367
colorToInternal DiscordColorDarkGold = 12745742
colorToInternal DiscordColorOrange = 15105570
colorToInternal DiscordColorDarkOrange = 11027200
colorToInternal DiscordColorRed = 15158332
colorToInternal DiscordColorDarkRed = 10038562
colorToInternal DiscordColorGray = 9807270
colorToInternal DiscordColorDarkGray = 9936031
colorToInternal DiscordColorDarkerGray = 8359053
colorToInternal DiscordColorLightGray = 12370112
colorToInternal DiscordColorNavy = 3426654
colorToInternal DiscordColorDarkNavy = 2899536
colorToInternal DiscordColorYellow = 16776960
colorToInternal DiscordColorDiscordWhite = 16777215
colorToInternal DiscordColorDiscordBlurple = 5793266
colorToInternal DiscordColorDiscordGrayple = 10070709
colorToInternal DiscordColorDiscordDarkButNotBlack = 2895667
colorToInternal DiscordColorDiscordNotQuiteBlack = 2303786
colorToInternal DiscordColorDiscordGreen = 5763719
colorToInternal DiscordColorDiscordYellow = 16705372
colorToInternal DiscordColorDiscordFuschia = 15418782
colorToInternal DiscordColorDiscordRed = 15548997
colorToInternal DiscordColorDiscordBlack = 16777215

-- | Convert a color integer to a RGB color with values in @[0..255]@
convertToRGB :: Integer -> DiscordColor
convertToRGB i = DiscordColorRGB (div i (256 * 256) .&. 255) (div i 256 .&. 255) (i .&. 255)

instance InternalDiscordEnum DiscordColor where
  discordTypeStartValue = DiscordColorDefault
  fromDiscordType = fromIntegral . colorToInternal
  discordTypeTable = map (\d -> (fromDiscordType d, d)) (makeTable discordTypeStartValue)
    where
      makeTable :: Data b => b -> [b]
      makeTable t = map (fromConstrB (fromConstr (toConstr (0 :: Int)))) (dataTypeConstrs $ dataTypeOf t)

instance ToJSON DiscordColor where
  toJSON = toJSON . fromDiscordType

instance FromJSON DiscordColor where
  parseJSON =
    withScientific
      "DiscordColor"
      ( \v ->
          discordTypeParseJSON "DiscordColor" (Number v)
            <|> ( case maybeInt v >>= Just . convertToRGB of
                    Nothing -> fail $ "could not parse discord color: " ++ show v
                    Just d -> return d
                )
      )
    where
      maybeInt i
        | fromIntegral (round i) == i = Just $ round i
        | otherwise = Nothing
