{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Provides actions for Channel API interactions
module Discord.Rest.User
  ( EmojiRequest(..)
  , ModifyGuildEmojiOpts(..)
  ) where

import Data.Aeson
import Data.Monoid (mempty, (<>))
import Network.HTTP.Req ((/:))
import qualified Network.HTTP.Req as R
import qualified Data.Text as T

import Discord.Rest.Prelude
import Discord.Types

instance Request (EmojiRequest a) where
  majorRoute = userMajorRoute
  jsonRequest = userJsonRequest


-- | Data constructor for requests. See <https://discordapp.com/developers/docs/resources/ API>
data EmojiRequest a where
  -- | List of emoji objects for the given guild. Requires MANAGE_EMOJIS permission.
  ListGuildEmojis :: Snowflake -> EmojiRequest [Emoji]
  -- | Emoji object for the given guild and emoji ID
  GetGuildEmoji :: Snowflake -> Snowflake -> EmojiRequest Emoji
  -- | Create a new guild emoji. Requires MANAGE_EMOJIS permission.
  -- todo CreateGuildEmoji :: Snowflake -> T.Text -> Image128x128 -> EmojiRequest Emoji
  -- | Requires MANAGE_EMOJIS permission
  ModifyGuildEmoji :: Snowflake -> Snowflake -> ModifyGuildEmojiOpts -> EmojiRequest Emoji
  -- | Requires MANAGE_EMOJIS permission
  DeleteGuildEmoji :: Snowflake -> Snowflake -> EmojiRequest ()

data ModifyGuildEmojiOpts = ModifyGuildEmojiOpts
     { modifyGuildEmojiName  :: T.Text
     , modifyGuildEmojiRoles :: [Snowflake]
     }

instance ToJSON ModifyGuildEmojiOpts where
  toJSON (ModifyGuildEmojiOpts name roles) =
    object [ "name" .= name, "roles" .= roles ]

userMajorRoute :: EmojiRequest a -> String
userMajorRoute c = case c of
  (ListGuildEmojis g) ->      "emoji " <> show g
  (GetGuildEmoji g _) ->      "emoji " <> show g
  -- todo (CreateGuildEmoji g) ->   "emoji " <> show g
  (ModifyGuildEmoji g _ _) -> "emoji " <> show g
  (DeleteGuildEmoji g _)   -> "emoji " <> show g

-- | The base url (Req) for API requests
baseUrl :: R.Url 'R.Https
baseUrl = R.https "discordapp.com" R./: "api" R./: apiVersion
  where apiVersion = "v6"

guilds :: R.Url 'R.Https
guilds = baseUrl /: "guilds"

userJsonRequest :: EmojiRequest r -> JsonRequest
userJsonRequest c = case c of
  (ListGuildEmojis g) -> Get (guilds // g) mempty
  (GetGuildEmoji g e) -> Get (guilds // g /: "emojis" // e) mempty
  -- todo (CreateGuildEmoji g) -> Post ()
  (ModifyGuildEmoji g e o) -> Patch (guilds // g /: "emojis" // e)
                                    (R.ReqBodyJson o)
                                    mempty
  (DeleteGuildEmoji g e) -> Delete (guilds // g /: "emojis" // e) mempty

