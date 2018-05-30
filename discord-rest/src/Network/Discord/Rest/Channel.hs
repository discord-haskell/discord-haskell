{-# LANGUAGE GADTs, OverloadedStrings, InstanceSigs, TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Provides actions for Channel API interactions
module Network.Discord.Rest.Channel
  (
    ChannelRequest(..)
  ) where

import Data.Aeson
import Data.ByteString.Lazy
import Data.Hashable
import Data.Monoid (mempty, (<>))
import qualified Data.Text as T
import Network.HTTP.Client (RequestBody (..))
import Network.HTTP.Client.MultipartFormData (partFileRequestBody)
import Network.HTTP.Req (reqBodyMultipart)

import Network.Discord.Rest.Prelude
import Network.Discord.Types
import Network.Discord.Rest.HTTP

-- | Data constructor for Channel requests. See <https://discordapp.com/developers/docs/resources/Channel Channel API>
data DiscordRequest a where
  -- | Gets a channel by its id.
  GetChannel              :: Snowflake -> DiscordRequest Channel
  -- | Edits channels options.
  ModifyChannel           :: ToJSON o  => Snowflake -> o -> DiscordRequest Channel
  -- | Deletes a channel if its id doesn't equal to the id of guild.
  DeleteChannel           :: Snowflake -> DiscordRequest Channel
  -- | Gets a messages from a channel with limit of 100 per request.
  GetChannelMessages      :: Snowflake -> Range -> DiscordRequest [Message]
  -- | Gets a message in a channel by its id.
  GetChannelMessage       :: Snowflake -> Snowflake -> DiscordRequest Message
  -- | Sends a message to a channel.
  CreateMessage           :: Snowflake -> T.Text -> Maybe Embed -> DiscordRequest Message
  -- | Sends a message with a file to a channel.
  UploadFile              :: Snowflake -> FilePath -> ByteString -> DiscordRequest Message
  -- | Edits a message content.
  EditMessage             :: Message   -> T.Text -> Maybe Embed -> DiscordRequest Message
  -- | Deletes a message.
  DeleteMessage           :: Message   -> DiscordRequest ()
  -- | Deletes a group of messages.
  BulkDeleteMessage       :: Snowflake -> [Message] -> DiscordRequest ()
  -- | Edits a permission overrides for a channel.
  EditChannelPermissions  :: ToJSON o  => Snowflake -> Snowflake -> o -> DiscordRequest ()
  -- | Gets all instant invites to a channel.
  GetChannelInvites       :: Snowflake -> DiscordRequest Object
  -- | Creates an instant invite to a channel.
  CreateChannelInvite     :: ToJSON o  => Snowflake -> o -> DiscordRequest Object
  -- | Deletes a permission override from a channel.
  DeleteChannelPermission :: Snowflake -> Snowflake -> DiscordRequest ()
  -- | Sends a typing indicator a channel which lasts 10 seconds.
  TriggerTypingIndicator  :: Snowflake -> DiscordRequest ()
  -- | Gets all pinned messages of a channel.
  GetPinnedMessages       :: Snowflake -> DiscordRequest [Message]
  -- | Pins a message.
  AddPinnedMessage        :: Snowflake -> Snowflake -> DiscordRequest ()
  -- | Unpins a message.
  DeletePinnedMessage     :: Snowflake -> Snowflake -> DiscordRequest ()

maybeEmbed :: Maybe Embed -> [(T.Text, Value)]
maybeEmbed [] = []
maybeEmbed em = ["embed" .= em]

authHeader :: DiscordAuth -> R.Option R.Https
authHeader (DiscordAuth auth version) = R.header "Authorization" auth
                                     <> R.header "User-Agent" agent
  where
  url = "https://github.com/jano017/Discord.hs"
  agent = "DiscordBot (" <> url <> ", " <> version <> ")"

-- Append to an URL
infixl 5 //
(//) :: Show a => R.Url scheme -> a -> R.Url scheme
url // part = url /: (T.pack $ show part)

executeRequest :: FromJSON a => DiscordAuth -> Request a -> IO (R.JsonResponse a)
executeRequest auth r = do (m, u, b) <- extractReq
                           R.req m u b R.jsonResponse (authHeader auth)
  where
  extractReq = case r of
      GetChannel chan               -> pure (R.GET, url // chan, R.NoReqBody)
      ModifyChannel chan            -> pure (R.GET, url // chan, R.NoReqBody)
      GetChannel chan               -> pure (R.GET, url // chan, R.NoReqBody)
      ModifyChannel chan patch      -> pure (R.PATH, url // chan, ReqBodyJson patch)
      DeleteChannel chan            -> pure (R.Delete, url // chan, R.NoReqBody)
      GetChannelMessages chan range -> pure (R.GET, url // chan /: "messages", toQueryString range)
      GetChannelMessage chan msg    -> pure (R.GET, url // chan /: "messages" // msg, R.NoReqBody)
      GetChannelInvites chan        -> pure (R.GET, url // chan /: "invites", R.NoReqBody)
      CreateChannelInvite chan patch -> pure (R.POST, url // chan /: "invites", ReqBodyJson patch)
      DeleteChannelPermission chan perm -> pure (R.DELETE, url // chan /: "permissions" // perm, R.NoReqBody)
      TriggerTypingIndicator chan   -> pure (R.POST, url // chan /: "typing", R.NoReqBody)
      GetPinnedMessages chan        -> pure (R.GET, url // chan /: "pins", R.NoReqBody)
      AddPinnedMessage chan msg     -> pure (R.PUT, url // chan /: "pins" // msg, R.NoReqBody)
      DeletePinnedMessage chan msg  -> pure (R.DELETE, url // chan /: "pins" // msg, R.NoReqBody)
      CreateMessage chan msg embed  -> pure (R.POST, url // chan /: "messages",
                                               ReqBodyJson . object $ ["content" .= msg] <> maybeEmbed embed)
      UploadFile chan fileName file -> do
          body <- reqBodyMultipart [partFileRequestBody "file" fileName $ RequestBodyLBS file]
          pure (R.POST, url // chan /: "messages", body)

      EditMessage (Message msg chan _ _ _ _ _ _ _ _ _ _ _ _) new embed -> pure
        (R.PATCH, url // chan /: "messages" // msg,
           ReqBodyJson . object $ ["content" .= new] <> maybeEmbed embed)
      DeleteMessage (Message msg chan _ _ _ _ _ _ _ _ _ _ _ _) new embed) -> pure
        (R.DELETE, url // chan /: "messages" // msg, R.NoReqBody)
      BulkDeleteMessage chan msgs -> pure
        (R.POST, url // chan /: "messages" /: "bulk-delete",
           ReqBodyJson $ object ["messages" .= map messageId msgs])
      EditChannelPermissions chan perm patch -> pure (R.Put, url // chan /: "permissions" // perm,
                                                       ReqBodyJson patch)

