{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Data structures pertaining to Discord Embed
module Discord.Internal.Types.Embed where

import Data.Aeson
import Data.Time.Clock
import Data.Default (Default, def)
import qualified Data.Text as T
import qualified Data.ByteString as B

import Discord.Internal.Types.Prelude

createEmbed :: CreateEmbed -> Embed
createEmbed CreateEmbed{..} =
  let
    emptyMaybe :: T.Text -> Maybe T.Text
    emptyMaybe t = if T.null t then Nothing else Just t

    embedImageToUrl :: T.Text -> CreateEmbedImage -> T.Text
    embedImageToUrl place cei = case cei of
                            CreateEmbedImageUrl t -> t
                            CreateEmbedImageUpload _ -> "attachment://" <> place <> ".png"

    embedAuthor = EmbedAuthor (emptyMaybe createEmbedAuthorName)
                              (emptyMaybe createEmbedAuthorUrl)
                              (embedImageToUrl "author" <$> createEmbedAuthorIcon)
                              Nothing
    embedImage = EmbedImage   (embedImageToUrl "image" <$> createEmbedImage)
                              Nothing Nothing Nothing
    embedThumbnail = EmbedThumbnail (embedImageToUrl "thumbnail" <$> createEmbedThumbnail)
                              Nothing Nothing Nothing
    embedFooter = EmbedFooter createEmbedFooterText
                              (embedImageToUrl "footer" <$> createEmbedFooterIcon)
                              Nothing

  in Embed { embedAuthor      = Just embedAuthor
           , embedTitle       = emptyMaybe createEmbedTitle
           , embedUrl         = emptyMaybe createEmbedUrl
           , embedThumbnail   = Just embedThumbnail
           , embedDescription = emptyMaybe createEmbedDescription
           , embedFields      = createEmbedFields
           , embedImage       = Just embedImage
           , embedFooter      = Just embedFooter
           , embedColor       = createEmbedColor
           , embedTimestamp   = Nothing

           -- can't set these
           , embedType        = Nothing
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
  , createEmbedColor       :: Maybe ColorInteger
--, createEmbedTimestamp   :: Maybe UTCTime
  } deriving (Show, Eq, Ord)

data CreateEmbedImage = CreateEmbedImageUrl T.Text
                      | CreateEmbedImageUpload B.ByteString
  deriving (Show, Eq, Ord)

instance Default CreateEmbed where
 def = CreateEmbed "" "" Nothing "" "" Nothing "" [] Nothing "" Nothing Nothing  -- Nothing

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
  , embedColor       :: Maybe ColorInteger    -- ^ The embed color
  , embedTimestamp   :: Maybe UTCTime    -- ^ The time of the embed content
  , embedType        :: Maybe T.Text     -- ^ Type of embed (Always "rich" for users)
  , embedVideo       :: Maybe EmbedVideo -- ^ Only present for "video" types
  , embedProvider    :: Maybe EmbedProvider -- ^ Only present for "video" types
  } deriving (Show, Eq, Ord)

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
   , "type"        .= embedType
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
          <*> o .:? "type"
          <*> o .:? "video"
          <*> o .:? "provider"


data EmbedThumbnail = EmbedThumbnail
  { embedThumbnailUrl :: Maybe T.Text
  , embedThumbnailProxyUrl :: Maybe T.Text
  , embedThumbnailHeight :: Maybe Integer
  , embedThumbnailWidth :: Maybe Integer
  } deriving (Show, Eq, Ord)

instance ToJSON EmbedThumbnail where
  toJSON (EmbedThumbnail a b c d) = object
    [ "url" .= a
    , "proxy_url" .= b
    , "height" .= c
    , "width" .= d
    ]

instance FromJSON EmbedThumbnail where
  parseJSON = withObject "thumbnail" $ \o ->
    EmbedThumbnail <$> o .:? "url"
                   <*> o .:? "proxy_url"
                   <*> o .:? "height"
                   <*> o .:? "width"

data EmbedVideo = EmbedVideo
  { embedVideoUrl :: Maybe T.Text
  , embedVideoHeight :: Maybe Integer
  , embedVideoWidth :: Maybe Integer
  } deriving (Show, Eq, Ord)

instance ToJSON EmbedVideo where
  toJSON (EmbedVideo a b c) = object
    [ "url" .= a
    , "height" .= b
    , "width" .= c
    ]

instance FromJSON EmbedVideo where
  parseJSON = withObject "video" $ \o ->
    EmbedVideo <$> o .:? "url"
               <*> o .:? "height"
               <*> o .:? "width"

data EmbedImage = EmbedImage
  { embedImageUrl :: Maybe T.Text
  , embedImageProxyUrl :: Maybe T.Text
  , embedImageHeight :: Maybe Integer
  , embedImageWidth :: Maybe Integer
  } deriving (Show, Eq, Ord)

instance ToJSON EmbedImage where
  toJSON (EmbedImage a b c d) = object
    [ "url" .= a
    , "proxy_url" .= b
    , "height" .= c
    , "width" .= d
    ]

instance FromJSON EmbedImage where
  parseJSON = withObject "image" $ \o ->
    EmbedImage <$> o .:? "url"
               <*> o .:? "proxy_url"
               <*> o .:? "height"
               <*> o .:? "width"

data EmbedProvider = EmbedProvider
  { embedProviderName :: Maybe T.Text
  , embedProviderUrl :: Maybe T.Text
  } deriving (Show, Eq, Ord)

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
  { embedAuthorName :: Maybe T.Text
  , embedAuthorUrl :: Maybe T.Text
  , embedAuthorIconUrl :: Maybe T.Text
  , embedAuthorProxyIconUrl :: Maybe T.Text
  } deriving (Show, Eq, Ord)

instance ToJSON EmbedAuthor where
  toJSON (EmbedAuthor a b c d) = object
    [ "name" .= a
    , "url" .= b
    , "icon_url" .= c
    , "proxy_icon_url" .= d
    ]

instance FromJSON EmbedAuthor where
  parseJSON = withObject "author" $ \o ->
    EmbedAuthor <$> o .:? "name"
                <*> o .:? "url"
                <*> o .:? "icon_url"
                <*> o .:? "proxy_icon_url"

data EmbedFooter = EmbedFooter
  { embedFooterText :: T.Text
  , embedFooterIconUrl :: Maybe T.Text
  , embedFooterProxyIconUrl :: Maybe T.Text
  } deriving (Show, Eq, Ord)

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
  } deriving (Show, Eq, Ord)

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
