{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

import Data.Char (isSpace)

import Data.Time
import Data.Aeson
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as Q

import Network.Discord.Rest

a :: IO ()
a = do
  tok <- Q.filter (not . isSpace) <$> Q.readFile "./examples/auth-token.secret"
  handle <- createHandler (DiscordAuth (Bot tok) "0.0.8")

  restCall handle $ CreateMessage 453207241294610444 "A" Nothing
  -- restCall handle $ GetChannel 453207241294610444

  --Resp (Text _ _ _ _ _ _ last) <- restCall handle $ GetChannel 453207241294610444
  --Resp msgs <- restCall handle $ GetChannelMessages 453207241294610444 (100, Before (last :: Snowflake))
  --restCall $ BulkDeleteMessage (453207241294610444, map messageId msgs)
  return ()


