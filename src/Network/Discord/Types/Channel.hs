{-# LANGUAGE OverloadedStrings, MultiWayIf #-}
module Network.Discord.Types.Channel where
  import Data.Foldable (asum)
  import Control.Monad (mzero)
  import Data.Text (pack)

  import Data.Time.Clock
  import Data.Aeson
  import Data.Vector (toList)
  import qualified Data.HashMap.Strict as HM

  import Network.Discord.Types.Global

  data User = User
    Snowflake
    String
    String
    (Maybe String)
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
           <*> o .:? "avatar"
           <*> o .:? "bot" .!= False
           <*> o .:? "mfa_enabled"
           <*> o .:? "verified"
           <*> o .:? "email"
    parseJSON _ = mzero

  data Channel =
      Text
        Snowflake
        Snowflake
        String
        String
        Integer
        [Overwrite]
        String
        Snowflake
    | Voice
        Snowflake
        Snowflake
        String
        String
        Integer
        [Overwrite]
        Integer
        Integer
    | DirectMessage
        Snowflake
        User
        Snowflake
    deriving (Show, Eq)

  instance FromJSON Channel where
    parseJSON = withObject "text or voice" $ \o -> do
      private <- o .: "is_private"
      if private then
        asum [
          Text  <$> o .: "id"
                <*> o .: "guild_id"
                <*> o .: "name"
                <*> o .: "type"
                <*> o .: "position"
                <*> o .: "permission_overwrites"
                <*> o .: "topic"
                <*> o .: "last_message_id"
         ,Voice <$> o .: "id"
                <*> o .: "guild_id"
                <*> o .: "name"
                <*> o .: "type"
                <*> o .: "position"
                <*> o .: "permission_overwrites"
                <*> o .: "topic"
                <*> o .: "last_message_id"
         ]
      else DirectMessage <$> o .: "id"
                         <*> o .: "recipient"
                         <*> o .: "last_message_id"
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
    [SubEmbed]
    deriving (Show, Eq)

  instance FromJSON Embed where
    parseJSON (Object o) =
      Embed <$> o .: "title"
            <*> o .: "type"
            <*> o .: "description"
            <*> o .: "url"
            <*> sequence (HM.foldrWithKey to_embed [] o)
      where
        to_embed k (Object v) a
          | k == pack "footer" =
            (Footer <$> v .: "text"
                    <*> v .: "icon_url"
                    <*> v .: "proxy_icon_url") : a
          | k == pack "image" =
            (Image <$> v .: "url"
                   <*> v .: "proxy_url"
                   <*> v .: "height"
                   <*> v .: "width") : a
          | k == pack "thumbnail" =
            (Thumbnail <$> v .: "url"
                       <*> v .: "proxy_url"
                       <*> v .: "height"
                       <*> v .: "width") : a
          | k == pack "video" =
            (Video <$> v .: "url"
                   <*> v .: "height"
                   <*> v .: "width") : a
          | k == pack "provider" =
            (Provider <$> v .: "name"
                      <*> v .: "url") : a
          | k == pack "author" =
            (Author <$> v .: "name"
                    <*> v .: "url"
                    <*> v .: "icon_url"
                    <*> v .: "proxy_icon_url") : a
        to_embed k (Array v) a
          | k == pack "fields" =
            [Field <$> i .: "name"
                   <*> i .: "value"
                   <*> i .: "inline"
                   | Object i <- toList v] ++ a
        to_embed _ _ a = a

    parseJSON _ = mzero

  data SubEmbed =
      Thumbnail
        String
        String
        Integer
        Integer
    | Video
        String
        Integer
        Integer
    | Image
        String
        String
        Integer
        Integer
    | Provider
        String
        String
    | Author
        String
        String
        String
        String
    | Footer
        String
        String
        String
    | Field
        String
        String
        Bool
    deriving (Show, Eq)
