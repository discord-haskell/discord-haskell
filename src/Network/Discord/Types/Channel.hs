{-# LANGUAGE OverloadedStrings, MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}
-- | Data structures pertaining to Discord Channels
module Network.Discord.Types.Channel where
  import Control.Monad (mzero)
  import Data.Text as Text (pack, Text)

  import Data.Aeson
  import Data.Aeson.Types (Parser)
  import Data.Time.Clock
  import Data.Vector (toList)
  import qualified Data.HashMap.Strict as HM
  import qualified Data.Vector as V

  import Network.Discord.Types.Prelude

  -- |Represents information about a user.
  data User = User
    { userId       :: {-# UNPACK #-} !Snowflake -- ^ The user's id.
    , userName     :: String                    -- ^ The user's username, not unique across
                                                --   the platform.
    , userDiscrim  :: String                    -- ^ The user's 4-digit discord-tag.
    , userAvatar   :: Maybe String              -- ^ The user's avatar hash.
    , userIsBot    :: Bool                      -- ^ Whether the user belongs to an OAuth2
                                                --   application.
    , userMfa      :: Maybe Bool                -- ^ Whether the user has two factor
                                                --   authentication enabled on the account.
    , userVerified :: Maybe Bool                -- ^ Whether the email on this account has
                                                --   been verified.
    , userEmail    :: Maybe String              -- ^ The user's email.
    } 
    | Webhook deriving (Show, Eq)

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

  -- | Guild channels represent an isolated set of users and messages in a Guild (Server)
  data Channel
    -- | A text channel in a guild.
    = Text
        { channelId          :: Snowflake   -- ^ The id of the channel (Will be equal to
                                            --   the guild if it's the "general" channel).
        , channelGuild       :: Snowflake   -- ^ The id of the guild.
        , channelName        :: String      -- ^ The name of the guild (2 - 1000 characters).
        , channelPosition    :: Integer     -- ^ The storing position of the channel.
        , channelPermissions :: [Overwrite] -- ^ An array of permission 'Overwrite's
        , channelTopic       :: String      -- ^ The topic of the channel. (0 - 1024 chars).
        , channelLastMessage :: Snowflake   -- ^ The id of the last message sent in the
                                            --   channel
        }
    -- |A voice channel in a guild.
    | Voice
        { channelId:: Snowflake
        , channelGuild:: Snowflake
        , channelName:: String
        , channelPosition:: Integer
        , channelPermissions:: [Overwrite]
        , channelBitRate:: Integer   -- ^ The bitrate (in bits) of the channel.
        , channelUserLimit:: Integer -- ^ The user limit of the voice channel.
        }
    -- | DM Channels represent a one-to-one conversation between two users, outside the scope
    --   of guilds
    | DirectMessage 
        { channelId          :: Snowflake
        , channelRecipients  :: [User]    -- ^ The 'User' object(s) of the DM recipient(s).
        , channelLastMessage :: Snowflake
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
  data Overwrite = Overwrite 
    { overwriteId:: {-# UNPACK #-} !Snowflake -- ^ 'Role' or 'User' id
    , overWriteType:: String                  -- ^ Either "role" or "member
    , overwriteAllow:: Integer                -- ^ Allowed permission bit set
    , overwriteDeny:: Integer                 -- ^ Denied permission bit set
    } deriving (Show, Eq)

  instance FromJSON Overwrite where
    parseJSON (Object o) =
      Overwrite <$> o .: "id"
                <*> o .: "type"
                <*> o .: "allow"
                <*> o .: "deny"
    parseJSON _ = mzero

  -- | Represents information about a message in a Discord channel.
  data Message = Message
    { messageId           :: {-# UNPACK #-} !Snowflake -- ^ The id of the message
    , messageChannel      :: {-# UNPACK #-} !Snowflake -- ^ Id of the channel the message
                                                       --   was sent in
    , messageAuthor       :: User                      -- ^ The 'User' the message was sent
                                                       --   by
    , messageContent      :: Text                      -- ^ Contents of the message
    , messageTimestamp    :: UTCTime                   -- ^ When the message was sent
    , messageEdited       :: Maybe UTCTime             -- ^ When/if the message was edited
    , messageTts          :: Bool                      -- ^ Whether this message was a TTS
                                                       --   message
    , messageEveryone     :: Bool                      -- ^ Whether this message mentions
                                                       --   everyone
    , messageMentions     :: [User]                    -- ^ 'User's specifically mentioned in
                                                       --   the message
    , messageMentionRoles :: [Snowflake]               -- ^ 'Role's specifically mentioned in
                                                       --   the message
    , messageAttachments  :: [Attachment]              -- ^ Any attached files
    , messageEmbeds       :: [Embed]                   -- ^ Any embedded content
    , messageNonce        :: Maybe Snowflake           -- ^ Used for validating if a message
                                                       --   was sent
    , messagePinned       :: Bool                      -- ^ Whether this message is pinned
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
  data SubEmbed
    = Thumbnail
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
