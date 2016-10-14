{-# LANGUAGE OverloadedStrings #-}
module Network.Discord.Types where
  import Data.Time.Clock
  import Data.Aeson
  import Data.Foldable
  import Control.Monad (mzero)

  type Snowflake = String
  
  data User = User
    Snowflake
    String
    String
    String
    Bool
    (Maybe Bool)
    (Maybe Bool)
    (Maybe String)
    deriving (Show, Eq)

  instance FromJSON User where
    parseJSON (Object o) = 
      User <$> o .:  "id"
           <*> o .:  "username"
           <*> o .:  "discriminator"
           <*> o .:  "avatar"
           <*> o .:? "bot" .!= False
           <*> o .:? "mfa_enabled"
           <*> o .:? "verified"
           <*> o .:? "email"
    parseJSON _ = mzero

  data Channel = Text
    Snowflake
    Snowflake
    String
    String
    Integer
    Bool
    [Overwrite]
    String
    Snowflake
               | Voice
    Snowflake
    Snowflake
    String
    String
    Integer
    Bool
    [Overwrite]
    Integer
    Integer
    deriving (Show, Eq)

  instance FromJSON Channel where
    parseJSON = withObject "text or voice" $ \o -> asum [
      Text  <$> o .: "id"
            <*> o .: "guild_id"
            <*> o .: "name"
            <*> o .: "type"
            <*> o .: "position"
            <*> o .: "is_private"
            <*> o .: "permission_overwrites"
            <*> o .: "topic"
            <*> o .: "last_message_id"
     ,Voice <$> o .: "id" 
            <*> o .: "guild_id"
            <*> o .: "name"
            <*> o .: "type"
            <*> o .: "position"
            <*> o .: "is_private"
            <*> o .: "permission_overwrites"
            <*> o .: "topic"
            <*> o .: "last_message_id"
     ]

  data Overwrite = Overwrite
    Snowflake
    String
    Integer
    Integer
    deriving (Show, Eq)

  instance FromJSON Overwrite where
    parseJSON (Object o) =
      Overwrite <$> o .: "id"
                <*> o .: "type"
                <*> o .: "allow"
                <*> o .: "deny"
    parseJSON _ = mzero

  data Message = Message
    Snowflake
    Snowflake
    User
    String
    UTCTime
    (Maybe UTCTime)
    Bool
    Bool
    [User]
    [Snowflake]
    [Attachment]
    [Embed]
    (Maybe Snowflake)
    Bool
    deriving (Show, Eq)

  instance FromJSON Message where
    parseJSON (Object o) =
      Message <$> o .:  "id"
              <*> o .:  "channel_id"
              <*> o .:  "author"
              <*> o .:  "content"
              <*> o .:  "timestamp"
              <*> o .:? "edited_timestamp"
              <*> o .:  "tts"
              <*> o .:  "mention_everyone"
              <*> o .:  "mentions"
              <*> o .:  "mention_roles"
              <*> o .:  "attachments"
              <*> o .:  "embeds"
              <*> o .:? "nonce"
              <*> o .:  "pinned"
    parseJSON _ = mzero

  data Attachment = Attachment
    Snowflake
    String
    Integer
    String
    String
    (Maybe Integer)
    (Maybe Integer)
    deriving (Show, Eq)
    
  instance FromJSON Attachment where
    parseJSON (Object o) = 
      Attachment <$> o .:  "id"
                 <*> o .:  "filename"
                 <*> o .:  "size"
                 <*> o .:  "url"
                 <*> o .:  "proxy_url"
                 <*> o .:? "height"
                 <*> o .:? "width"
    parseJSON _ = mzero

  data Embed = Embed
    String
    String
    String
    String
    Thumbnail
    Provider
    deriving (Show, Eq)

  instance FromJSON Embed where
    parseJSON (Object o) =
      Embed <$> o .: "title"
            <*> o .: "type"
            <*> o .: "description"
            <*> o .: "url"
            <*> o .: "thumbnail"
            <*> o .: "provider"
    parseJSON _ = mzero

  data Thumbnail = Thumbnail
    String
    String
    Integer
    Integer
    deriving (Show, Eq)

  instance FromJSON Thumbnail where
    parseJSON (Object o) =
      Thumbnail <$> o .: "url"
                <*> o .: "proxy_url"
                <*> o .: "height"
                <*> o .: "width"
    parseJSON _ = mzero

  data Provider = Provider
    String
    String
    deriving (Show, Eq)

  instance FromJSON Provider where
    parseJSON (Object o) =
      Provider <$> o .: "name"
               <*> o .: "url"
    parseJSON _ = mzero
