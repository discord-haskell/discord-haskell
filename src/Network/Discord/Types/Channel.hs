{-# LANGUAGE OverloadedStrings, MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}
module Network.Discord.Types.Channel where
  import Control.Monad (mzero)
  import Data.Text as Text (pack, Text)

  import Data.Time.Clock
  import Data.Aeson
  import Data.Aeson.Types (Parser)
  import Data.Vector (toList)
  import qualified Data.Vector as V
  import qualified Data.HashMap.Strict as HM

  import Network.Discord.Types.Prelude

  -- |Represents information about a user.
  data User = User {
      -- |A unique, timestamp based id.
      userId :: {-# UNPACK #-} !Snowflake,
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
    } 
    | Webhook deriving (Show, Eq)

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

  -- TODO: group DM support.
  -- | Guild channels represent an isolated set of users and messages in a Guild (Server)
  data Channel =
      -- | A text channel in a guild.
      Text{
        -- | The id of the channel (Will be equal to the guild if it's the "general" channel.
        channelId:: Snowflake,
        -- | The id of the guild.
        channelGuild:: Snowflake,
        -- | The name of the guild (2 - 1000 characters).
        channelName:: String,
        -- | The storing position of the channel.
        channelPosition:: Integer,
        -- | An array of 'Overwrite' objects.
        channelPermissions:: [Overwrite],
        -- | The channel's topic (2 - 1024 characters).
        channelTopic:: String,
        -- | The snowflake of last message sent.
        channelLastMessage:: Snowflake }
    -- |A voice channel in a guild.
    | Voice{
        -- | The id of the channel (Will be equal to the guild if it's the "general" channel.
        channelId:: Snowflake,
        -- | The id of the guild.
        channelGuild:: Snowflake,
        -- | The name of the guild (2 - 1000 characters).
        channelName:: String,
        -- | The storing position of the channel.
        channelPosition:: Integer,
        -- | An array of 'Overwrite' objects.
        channelPermissions:: [Overwrite],
        -- | The bitrate (in bits) of the voice channel.
        channelBitRate:: Integer,
        -- | The user limit of the voice channel.
        channelUserLimit:: Integer }
    -- | DM Channels represent a one-to-one conversation between two users, outside the scope
    --   of guilds
    | DirectMessage {
        -- | The id of this private message.
        channelId:: Snowflake,
        -- | The user object(s) of the DM recipient(s).
        channelRecipients:: [User],
        -- | The id of of the last message sent in this DM.
        channelLastMessage:: Snowflake
    } deriving (Show, Eq)

  instance FromJSON Channel where
    parseJSON = withObject "text or voice" $ \o -> do
      type' <- (o .: "type") :: Parser Int
      case type' of
        0 ->
            Text  <$> o .:  "id"
                  <*> o .:  "guild_id"
                  <*> o .:  "name"
                  <*> o .:  "position"
                  <*> o .:  "permission_overwrites"
                  <*> o .:? "topic" .!= ""
                  <*> o .:? "last_message_id" .!= 0
        1 ->
            DirectMessage <$> o .:  "id"
                          <*> o .:  "recipients"
                          <*> o .:? "last_message_id" .!= 0
        2 ->
            Voice <$> o .: "id"
                  <*> o .: "guild_id"
                  <*> o .: "name"
                  <*> o .: "position"
                  <*> o .: "permission_overwrites"
                  <*> o .: "bitrate"
                  <*> o .: "user_limit"
        _ -> mzero

  -- | Permission overwrites for a channel.
  data Overwrite = Overwrite {
    -- |An id of an overwrite's target which is a role or a member.
    overwriteId:: {-# UNPACK #-} !Snowflake,
    -- |A type of an overwrite target.
    overWriteType:: String,
    -- |Allowed permission bits for a channel.
    overwriteAllow:: Integer,
    -- |Denied permission bits for a channel.
    overwriteDeny:: Integer
    } deriving (Show, Eq)

  -- | Allows a channel's permissions datatype to be generated using a JSON response by Discord.
  instance FromJSON Overwrite where
    parseJSON (Object o) =
      Overwrite <$> o .: "id"
                <*> o .: "type"
                <*> o .: "allow"
                <*> o .: "deny"
    parseJSON _ = mzero

  -- | Represents information about a message in a Discord channel.
  data Message = Message{
    -- |A unique, timestamp based id.
    messageId:: {-# UNPACK #-} !Snowflake,
    -- |The Id of a target channel.
    messageChannel:: {-# UNPACK #-} !Snowflake,
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

  instance FromJSON Message where
    parseJSON (Object o) =
      Message <$> o .:  "id"
              <*> o .:  "channel_id"
              <*> o .:? "author" .!= Webhook
              <*> o .:? "content" .!= ""
              <*> o .:? "timestamp" .!= epochTime
              <*> o .:? "edited_timestamp"
              <*> o .:? "tts" .!= False
              <*> o .:? "mention_everyone" .!= False
              <*> o .:? "mentions" .!= []
              <*> o .:? "mention_roles" .!= []
              <*> o .:? "attachments" .!= []
              <*> o .:  "embeds"
              <*> o .:? "nonce"
              <*> o .:? "pinned" .!= False
    parseJSON _ = mzero

  -- |Represents an attached to a message file.
  data Attachment = Attachment
    { attachmentId       :: {-# UNPACK #-} !Snowflake -- ^ Attachment id
    , attachmentFilename :: String                    -- ^ Name of attached file
    , attachmentSize     :: Integer                   -- ^ Size of file (in bytes)
    , attachmentUrl      :: String                    -- ^ Source of file
    , attachmentProxy    :: String                    -- ^ Proxied url of file
    , attachmentHeight   :: Maybe Integer             -- ^ Height of file (if image)
    , attachmentWidth    :: Maybe Integer             -- ^ Width of file (if image)
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
    { embedTitle  :: String     -- ^ Title of the embed
    , embedType   :: String     -- ^ Type of embed (Always "rich" for webhooks)
    , embedDesc   :: String     -- ^ Description of embed
    , embedUrl    :: String     -- ^ URL of embed
    , embedTime   :: UTCTime    -- ^ The time of the embed content
    , embedColor  :: Integer    -- ^ The embed color
    , embedFields ::[SubEmbed]  -- ^ Fields of the embed
    } deriving (Show, Read, Eq)

  -- |Allows a message's embed to be generated using a JSON response by Discord.
  instance FromJSON Embed where
    parseJSON (Object o) = 
      Embed <$> o .:? "title" .!= "Untitled"
            <*> o .:  "type"
            <*> o .:? "description" .!= ""
            <*> o .:? "url" .!= ""
            <*> o .:? "timestamp" .!= epochTime
            <*> o .:? "color" .!= 0
            <*> sequence (HM.foldrWithKey to_embed [] o)
      where
        to_embed k (Object v) a
          | k == pack "footer" =
            (Footer <$> v .: "text"
                    <*> v .:? "icon_url" .!= ""
                    <*> v .:? "proxy_icon_url" .!= "") : a
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
                      <*> v .:? "url" .!= "") : a
          | k == pack "author" =
            (Author <$> v .:  "name"
                    <*> v .:?  "url" .!= ""
                    <*> v .:? "icon_url" .!= ""
                    <*> v .:? "proxy_icon_url" .!= "") : a
        to_embed k (Array v) a
          | k == pack "fields" =
            [Field <$> i .: "name"
                   <*> i .: "value"
                   <*> i .: "inline"
                   | Object i <- toList v] ++ a
        to_embed _ _ a = a

    parseJSON _ = mzero

  instance ToJSON Embed where
    toJSON (Embed {..}) = object 
      [ "title"       .= embedTitle
      , "type"        .= embedType
      , "description" .= embedDesc
      , "url"         .= embedUrl
      , "timestamp"   .= embedTime
      , "color"       .= embedColor
      ] |> makeSubEmbeds embedFields
      where
        (Object o) |> hm = Object $ HM.union o hm
        _ |> _ = error "Type mismatch"
        makeSubEmbeds = foldr embed HM.empty
        embed (Thumbnail url _ height width) =
          HM.alter (\_ -> Just $ object
            [ "url"    .= url
            , "height" .= height
            , "width"  .= width
            ]) "thumbnail"
        embed (Image url _ height width) = 
          HM.alter (\_ -> Just $ object
            [ "url"    .= url
            , "height" .= height
            , "width"  .= width
            ]) "image"
        embed (Author name url icon _) =
          HM.alter (\_ -> Just $ object
            [ "name"     .= name
            , "url"      .= url
            , "icon_url" .= icon
            ]) "author"
        embed (Footer text icon _) = 
          HM.alter (\_ -> Just $ object
            [ "text"     .= text
            , "icon_url" .= icon
            ]) "footer"
        embed (Field name value inline) =
          HM.alter (\val -> case val of
            Just (Array a) -> Just . Array $ V.cons (object
              [ "name"   .= name
              , "value"  .= value
              , "inline" .= inline
              ]) a
            _ -> Just $ toJSON [
              object
                [ "name"   .= name
                , "value"  .= value
                , "inline" .= inline
                ]
              ]
          ) "fields"
        embed _ = id

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
    deriving (Show, Read, Eq)
