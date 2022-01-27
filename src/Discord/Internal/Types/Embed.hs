{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}

-- | Data structures pertaining to Discord Embed
module Discord.Internal.Types.Embed where

import Data.Aeson
import Data.Time.Clock
import Data.Default (Default, def)
import qualified Data.Text as T
import qualified Data.ByteString as B
import Data.Functor ((<&>))
import Control.Applicative (Alternative((<|>)))

import Discord.Internal.Types.Prelude (InternalDiscordType(..))
import Data.Data
import Text.Read (readMaybe)
import Data.Maybe (fromMaybe)
import Data.Char (toLower)
import Data.Bits (Bits((.&.)))
import Network.HTTP.Client.MultipartFormData (PartM, partFileRequestBody)
import Network.HTTP.Client (RequestBody(RequestBodyBS))

createEmbed :: CreateEmbed -> Embed
createEmbed CreateEmbed{..} =
  let
    emptyMaybe :: T.Text -> Maybe T.Text
    emptyMaybe t = if T.null t then Nothing else Just t

    embedImageToUrl :: T.Text -> CreateEmbedImage -> T.Text
    embedImageToUrl place cei = case cei of
                            CreateEmbedImageUrl t -> t
                            CreateEmbedImageUpload _ -> T.filter (/=' ') $ "attachment://" <> createEmbedTitle <> place <> ".png"

    embedAuthor = EmbedAuthor createEmbedAuthorName
                              (emptyMaybe createEmbedAuthorUrl)
                              (embedImageToUrl "author" <$> createEmbedAuthorIcon)
                              Nothing
    embedImage = EmbedImage  <$> (embedImageToUrl "image" <$> createEmbedImage)
                              <&> ($ Nothing) <&> ($ Nothing) <&> ($ Nothing)
    embedThumbnail = EmbedThumbnail <$> (embedImageToUrl "thumbnail" <$> createEmbedThumbnail)
                                    <&> ($ Nothing) <&> ($ Nothing) <&> ($ Nothing)
    embedFooter = EmbedFooter createEmbedFooterText
                              (embedImageToUrl "footer" <$> createEmbedFooterIcon)
                              Nothing

  in Embed { embedAuthor      = Just embedAuthor
           , embedTitle       = emptyMaybe createEmbedTitle
           , embedUrl         = emptyMaybe createEmbedUrl
           , embedThumbnail   = embedThumbnail
           , embedDescription = emptyMaybe createEmbedDescription
           , embedFields      = createEmbedFields
           , embedImage       = embedImage
           , embedFooter      = Just embedFooter
           , embedColor       = createEmbedColor
           , embedTimestamp   = createEmbedTimestamp

           -- can't set these
           , embedVideo       = Nothing
           , embedProvider    = Nothing
           }

data CreateEmbed = CreateEmbed
  { createEmbedAuthorName  :: T.Text
  , createEmbedAuthorUrl   :: T.Text
  , createEmbedAuthorIcon  :: Maybe CreateEmbedImage
  , createEmbedTitle       :: T.Text
  , createEmbedUrl         :: T.Text
  , createEmbedThumbnail   :: Maybe CreateEmbedImage
  , createEmbedDescription :: T.Text
  , createEmbedFields      :: [EmbedField]
  , createEmbedImage       :: Maybe CreateEmbedImage
  , createEmbedFooterText  :: T.Text
  , createEmbedFooterIcon  :: Maybe CreateEmbedImage
  , createEmbedColor       :: Maybe DiscordColor
  , createEmbedTimestamp   :: Maybe UTCTime
  } deriving (Show, Read, Eq, Ord)

data CreateEmbedImage = CreateEmbedImageUrl T.Text
                      | CreateEmbedImageUpload B.ByteString
  deriving (Show, Read, Eq, Ord)

instance Default CreateEmbed where
 def = CreateEmbed "" "" Nothing "" "" Nothing "" [] Nothing "" Nothing Nothing Nothing

-- | An embed attached to a message.
data Embed = Embed
  { embedAuthor      :: Maybe EmbedAuthor
  , embedTitle       :: Maybe T.Text     -- ^ Title of the embed
  , embedUrl         :: Maybe T.Text     -- ^ URL of embed
  , embedThumbnail   :: Maybe EmbedThumbnail -- ^ Thumbnail in top-right
  , embedDescription :: Maybe T.Text     -- ^ Description of embed
  , embedFields      :: [EmbedField]     -- ^ Fields of the embed
  , embedImage       :: Maybe EmbedImage
  , embedFooter      :: Maybe EmbedFooter
  , embedColor       :: Maybe DiscordColor    -- ^ The embed color
  , embedTimestamp   :: Maybe UTCTime    -- ^ The time of the embed content
  , embedVideo       :: Maybe EmbedVideo -- ^ Only present for "video" types
  , embedProvider    :: Maybe EmbedProvider -- ^ Only present for "video" types
  } deriving (Show, Read, Eq, Ord)

-- TODO
instance ToJSON Embed where
  toJSON Embed{..} = object
   [ "author"      .= embedAuthor
   , "title"       .= embedTitle
   , "url"         .= embedUrl
   , "description" .= embedDescription
   , "thumbnail"   .= embedThumbnail
   , "fields"      .= embedFields
   , "image"       .= embedImage
   , "footer"      .= embedFooter
   , "color"       .= embedColor
   , "timestamp"   .= embedTimestamp
   , "video"       .= embedVideo
   , "provider"    .= embedProvider
    ]

instance FromJSON Embed where
  parseJSON = withObject "embed" $ \o ->
    Embed <$> o .:? "author"
          <*> o .:? "title"
          <*> o .:? "url"
          <*> o .:? "thumbnail"
          <*> o .:? "description"
          <*> o .:? "fields" .!= []
          <*> o .:? "image"
          <*> o .:? "footer"
          <*> o .:? "color"
          <*> o .:? "timestamp"
          <*> o .:? "video"
          <*> o .:? "provider"


data EmbedThumbnail = EmbedThumbnail
  { embedThumbnailUrl :: T.Text
  , embedThumbnailProxyUrl :: Maybe T.Text
  , embedThumbnailHeight :: Maybe Integer
  , embedThumbnailWidth :: Maybe Integer
  } deriving (Show, Read, Eq, Ord)

instance ToJSON EmbedThumbnail where
  toJSON (EmbedThumbnail a b c d) = object
    [ "url" .= a
    , "proxy_url" .= b
    , "height" .= c
    , "width" .= d
    ]

instance FromJSON EmbedThumbnail where
  parseJSON = withObject "thumbnail" $ \o ->
    EmbedThumbnail <$> o .: "url"
                   <*> o .:? "proxy_url"
                   <*> o .:? "height"
                   <*> o .:? "width"

data EmbedVideo = EmbedVideo
  { embedVideoUrl :: Maybe T.Text
  , embedProxyUrl :: Maybe T.Text
  , embedVideoHeight :: Maybe Integer
  , embedVideoWidth :: Maybe Integer
  } deriving (Show, Read, Eq, Ord)

instance ToJSON EmbedVideo where
  toJSON (EmbedVideo a a' b c) = object
    [ "url" .= a
    , "height" .= b
    , "width" .= c
    , "proxy_url" .= a'
    ]

instance FromJSON EmbedVideo where
  parseJSON = withObject "video" $ \o ->
    EmbedVideo <$> o .:? "url"
               <*> o .:? "proxy_url"
               <*> o .:? "height"
               <*> o .:? "width"

data EmbedImage = EmbedImage
  { embedImageUrl :: T.Text
  , embedImageProxyUrl :: Maybe T.Text
  , embedImageHeight :: Maybe Integer
  , embedImageWidth :: Maybe Integer
  } deriving (Show, Read, Eq, Ord)

instance ToJSON EmbedImage where
  toJSON (EmbedImage a b c d) = object
    [ "url" .= a
    , "proxy_url" .= b
    , "height" .= c
    , "width" .= d
    ]

instance FromJSON EmbedImage where
  parseJSON = withObject "image" $ \o ->
    EmbedImage <$> o .:  "url"
               <*> o .:? "proxy_url"
               <*> o .:? "height"
               <*> o .:? "width"

data EmbedProvider = EmbedProvider
  { embedProviderName :: Maybe T.Text
  , embedProviderUrl :: Maybe T.Text
  } deriving (Show, Read, Eq, Ord)

instance ToJSON EmbedProvider where
  toJSON (EmbedProvider a b) = object
    [ "name" .= a
    , "url" .= b
    ]

instance FromJSON EmbedProvider where
  parseJSON = withObject "provider" $ \o ->
    EmbedProvider <$> o .:? "name"
                  <*> o .:? "url"

data EmbedAuthor = EmbedAuthor
  { embedAuthorName :: T.Text
  , embedAuthorUrl :: Maybe T.Text
  , embedAuthorIconUrl :: Maybe T.Text
  , embedAuthorProxyIconUrl :: Maybe T.Text
  } deriving (Show, Read, Eq, Ord)

instance ToJSON EmbedAuthor where
  toJSON (EmbedAuthor a b c d) = object
    [ "name" .= a
    , "url" .= b
    , "icon_url" .= c
    , "proxy_icon_url" .= d
    ]

instance FromJSON EmbedAuthor where
  parseJSON = withObject "author" $ \o ->
    EmbedAuthor <$> o .:  "name"
                <*> o .:? "url"
                <*> o .:? "icon_url"
                <*> o .:? "proxy_icon_url"

data EmbedFooter = EmbedFooter
  { embedFooterText :: T.Text
  , embedFooterIconUrl :: Maybe T.Text
  , embedFooterProxyIconUrl :: Maybe T.Text
  } deriving (Show, Read, Eq, Ord)

instance ToJSON EmbedFooter where
  toJSON (EmbedFooter a b c) = object
    [ "text" .= a
    , "icon_url" .= b
    , "proxy_icon_url" .= c
    ]

instance FromJSON EmbedFooter where
  parseJSON = withObject "footer" $ \o ->
    EmbedFooter <$> o .:  "text"
                <*> o .:? "icon_url"
                <*> o .:? "proxy_icon_url"

data EmbedField = EmbedField
  { embedFieldName :: T.Text
  , embedFieldValue :: T.Text
  , embedFieldInline :: Maybe Bool
  } deriving (Show, Read, Eq, Ord)

instance ToJSON EmbedField where
  toJSON (EmbedField a b c) = object
    [ "name" .= a
    , "value" .= b
    , "inline" .= c
    ]

instance FromJSON EmbedField where
  parseJSON = withObject "field" $ \o ->
    EmbedField <$> o .:  "name"
               <*> o .:  "value"
               <*> o .:? "inline"

-- | Color names
-- Color is a bit of a mess on discord embeds.
-- I've here stolen the pallet list from https://gist.github.com/thomasbnt/b6f455e2c7d743b796917fa3c205f812
--
-- All discord embed color stuff is credited to
-- https://github.com/WarwickTabletop/tablebot/pull/34
data DiscordColor
  = DiscordColorRGB Integer Integer Integer
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

convertToRGB :: Integer -> DiscordColor
convertToRGB i = DiscordColorRGB (div i (256 * 256) .&. 255) (div i 256 .&. 255) (i .&. 255)

instance InternalDiscordType DiscordColor where
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

maybeEmbed :: Maybe CreateEmbed -> [PartM IO]
maybeEmbed = 
      let mkPart (name,content) = partFileRequestBody name (T.unpack name) (RequestBodyBS content)
          uploads CreateEmbed{..} = [(T.filter (/=' ') $ createEmbedTitle<>n,c) | (n, Just (CreateEmbedImageUpload c)) <-
                                          [ ("author.png", createEmbedAuthorIcon)
                                          , ("thumbnail.png", createEmbedThumbnail)
                                          , ("image.png", createEmbedImage)
                                          , ("footer.png", createEmbedFooterIcon) ]]
      in maybe [] (map mkPart . uploads)
