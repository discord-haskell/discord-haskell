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

  -- |Represents information about a user.
  data User = User {
      -- |A unique, timestamp based id.
      userId :: Snowflake,
      -- |The account's username. If changed, may cause discriminator to change.
      userName:: String,
      -- |A unique discriminator consisting of four numbers.
      userDiscrim:: String,
      -- |An optional avatar hash that's a part of url.
      userAvatar:: Maybe String,
      -- |True if a user is a bot account.
      userIsBot:: Bool,
      -- |If current user, true if 2-factor authentication is enabled.
      userMfa:: Maybe Bool,
      -- |If current user, true if current e-mail is verified.
      userVerified:: Maybe Bool,
      -- |If current user, a string containing e-mail.
      userEmail:: Maybe String
    } deriving (Show, Eq)

  -- |Allows a user datatype to be generated using a JSON response by Discord.
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

  -- |Represents information about possible variants of a Discord channel.
  -- TODO: group DM support.
  data Channel =
      -- |A text channel in a guild.
      Text{
        -- |A unique, timestamp based id.
        channelId:: Snowflake,
        -- |An id of guild that holds the channel.
        channelGuild:: Snowflake,
        -- |A name of the channel.
        channelName:: String,
        -- |A position of the channel.
        channelPosition:: Integer,
        -- |Permission overwrites of the channel.
        channelPermissions:: [Overwrite],
        -- |The channel's topic.
        channelTopic:: String,
        -- |The snowflake of last message sent, used by client to check for unread messages.
        channelLastMessage:: Snowflake }
    -- |A voice channel in a guild.
    | Voice{
        -- |A unique, timestamp based id.
        channelId:: Snowflake,
        -- |An id of guild that holds the channel.
        channelGuild:: Snowflake,
        -- |A name of the voice channel.
        channelName:: String,
        -- |A position of the channel.
        channelPosition:: Integer,
        -- |Permission overwrites of the channel.
        channelPermissions:: [Overwrite],
        -- |Bitrate of the channel.
        channelBitRate:: Integer,
        -- |Determines when discord should not allow a user to join this channel.
        channelUserLimit:: Integer }
    -- |A direct message channel between two users.
    | DirectMessage {
        -- |A unique, timestamp based id.
        channelId:: Snowflake,
        -- |The target user of a direct message.
        channelRecipient:: User,
        -- |The snowflake of last message sent, used by client to check for unread messages.
        channelLastMessage:: Snowflake
    } deriving (Show, Eq)

  -- |Allows a channel datatype to be generated using a JSON response by Discord.
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
  -- | Permission overwrites for a channel.
  data Overwrite = Overwrite {
    -- |An id of an overwrite's target which is a role or a member.
    overwriteId:: Snowflake,
    -- |A type of an overwrite target.
    overWriteType:: String,
    -- |Allowed permission bits for a channel.
    overwriteAllow:: Integer,
    -- |Denied permission bits for a channel.
    overwriteDeny:: Integer
    } deriving (Show, Eq)

  -- |Allows a channel's permissions datatype to be generated using a JSON response by Discord.
  instance FromJSON Overwrite where
    parseJSON (Object o) =
      Overwrite <$> o .: "id"
                <*> o .: "type"
                <*> o .: "allow"
                <*> o .: "deny"
    parseJSON _ = mzero

  -- |Represents information about a message in a Discord channel.
  data Message = Message{
    -- |A unique, timestamp based id.
    messageId:: Snowflake,
    -- |The Id of a target channel.
    messageChannel:: Snowflake,
    -- |The person that sent the message.
    messageAuthor:: User,
    -- |The content of message.
    messageContent:: Text,
    -- |Time when the message was sent.
    messageTimestamp:: UTCTime,
    -- |Time when the message was edited.
    messageEdited:: Maybe UTCTime,
    -- |True if message was sent with tts command.
    messageTts:: Bool,
    -- |True if message mentions here or everyone.
    messageEveryone:: Bool,
    -- |List of users mentioned by message.
    messageMentions:: [User],
    -- |List of roles mentioned by message.
    messageMentionRoles:: [Snowflake],
    -- |List of attached to message files.
    messageAttachments:: [Attachment],
    -- |List of embeds that the message contains.
    messageEmbeds:: [Embed],
    -- |A timestamp used to clarify message order.
    messageNonce:: Maybe Snowflake,
    -- |True if message is pinned in current channel.
    messagePinned:: Bool
    } deriving (Show, Eq)

  -- |Allows a message datatype to be generated using a JSON response by Discord.
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

  -- |Represents an attached to a message file.
  data Attachment = Attachment{
    -- |A unique, timestamp based id.
    attachmentId:: Snowflake,
    -- |The attachment's filename.
    attachmentFilename:: String,
    -- |The attachment's file size in bytes.
    attachmentSize:: Integer,
    -- |The CDN URL this attachment can be downloaded at.
    attachmentUrl:: String,
    -- |The attachment's proxy URL.
    attachmentProxy:: String,
    -- |If an image, the height of it, in pixels
    attachmentHeight:: Maybe Integer,
    -- |If an image, the width of it, in pixels
    attachmentWidth:: Maybe Integer
    } deriving (Show, Eq)

  -- |Allows a message's attachment to be generated using a JSON response by Discord.
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

  -- |An embed attached to a message.
  data Embed = Embed
    String
    String
    String
    String
    [SubEmbed]
    deriving (Show, Eq)

  -- |Allows a message's embed to be generated using a JSON response by Discord.
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

  -- |Represents a part of an embed.
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
