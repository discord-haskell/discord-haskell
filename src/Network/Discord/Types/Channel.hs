{-# LANGUAGE OverloadedStrings, MultiWayIf #-}
module Network.Discord.Types.Channel where
  import Control.Monad (mzero)
  import Data.Text as Text (pack, Text)

  import Data.Time.Clock
  import Data.Aeson
  import Data.Aeson.Types (Parser)
  import Data.Vector (toList)
  import qualified Data.HashMap.Strict as HM

  import Network.Discord.Types.Prelude

  data User = User {
      userId :: Snowflake,
      userName:: String,
      userDiscrim:: String,
      userAvatatr:: Maybe String,
      userIsBot:: Bool,
      userMfa:: Maybe Bool,
      userVerified:: Maybe Bool,
      userEmail:: Maybe String
    } deriving (Show, Eq)

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
      Text{
        channelId:: Snowflake,
        channelGuild:: Snowflake,
        channelName:: String,
        channelPosition:: Integer,
        channelPermissions:: [Overwrite],
        channelTopic:: String,
        channelLastMessage:: Snowflake }
    | Voice{
        channelId:: Snowflake,
        channelGuild:: Snowflake,
        channelName:: String,
        channelPosition:: Integer,
        channelPermissions:: [Overwrite],
        channelBitRate:: Integer,
        channelUserLimit:: Integer }
    | DirectMessage {
        channelId:: Snowflake,
        channelRecipient:: User,
        channelLastMessage:: Snowflake
    } deriving (Show, Eq)

  instance FromJSON Channel where
    parseJSON = withObject "text or voice" $ \o -> do
      private <- o .:? "is_private" .!= False
      if not private then do
        type' <- (o .: "type") :: Parser Text
        case type' of
          "text" ->
              Text  <$> o .: "id"
                    <*> o .: "guild_id"
                    <*> o .: "name"
                    <*> o .: "position"
                    <*> o .: "permission_overwrites"
                    <*> o .: "topic"
                    <*> o .: "last_message_id"
          "voice" ->
              Voice <$> o .: "id"
                    <*> o .: "guild_id"
                    <*> o .: "name"
                    <*> o .: "position"
                    <*> o .: "permission_overwrites"
                    <*> o .: "bitrate"
                    <*> o .: "user_limit"
          _ -> mzero

      else DirectMessage <$> o .: "id"
                         <*> o .: "recipients"
                         <*> o .: "last_message_id"
  data Overwrite = Overwrite {
    overwriteId:: Snowflake,
    overWriteType:: String,
    overwriteAllow:: Integer,
    overwriteDeny:: Integer
    } deriving (Show, Eq)

  instance FromJSON Overwrite where
    parseJSON (Object o) =
      Overwrite <$> o .: "id"
                <*> o .: "type"
                <*> o .: "allow"
                <*> o .: "deny"
    parseJSON _ = mzero

  data Message = Message{
    messageId:: Snowflake,
    messageChannel:: Snowflake,
    messageAuthor:: User,
    messageContent:: Text,
    messageTimestamp:: UTCTime,
    messageEdited:: Maybe UTCTime,
    messageTts:: Bool,
    messageEveryone:: Bool,
    messageMentions:: [User],
    messageMentionRoles:: [Snowflake],
    messageAttachments:: [Attachment],
    messageEmbeds:: [Embed],
    messageNonce:: Maybe Snowflake,
    messagePinned:: Bool
    } deriving (Show, Eq)

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

  data Attachment = Attachment{
    attachmentId:: Snowflake,
    attachmentFilename:: String,
    attachmentSize:: Integer,
    attachmentUrl:: String,
    attachmentProxy:: String,
    attachmentHeight:: Maybe Integer,
    attachmentWidth:: Maybe Integer
    } deriving (Show, Eq)

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
