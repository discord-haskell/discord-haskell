{-# LANGUAGE OverloadedStrings #-}
module Network.Discord.Types.Guild where
  import Data.Aeson
  import Control.Monad (mzero)

  import Network.Discord.Types.Channel
  import Network.Discord.Types.Prelude

  -- |Representation of a guild member.
  data Member = GuildMember Snowflake User
            | MemberShort User (Maybe String) [Snowflake]
            deriving Show
  instance FromJSON Member where
    parseJSON (Object o) =
      GuildMember <$> o .: "guild_id" <*> o .: "user"
    parseJSON _ = mzero

  -- Temporary representation of a guild, will be replaced
  data Guild =
    Guild Snowflake
    deriving Show
  instance FromJSON Guild where
    parseJSON (Object o) = Guild <$> o .: "id"
    parseJSON _          = mzero
