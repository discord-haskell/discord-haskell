{-# LANGUAGE GADTs, OverloadedStrings, InstanceSigs, TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, ScopedTypeVariables #-}
module Network.Discord.Rest.User
  (
    UserRequest(..)
  ) where
    import Data.Text as T
    import Data.Maybe (fromMaybe)
    import qualified Control.Monad.State as ST (get, put, liftIO)
    import Control.Monad.Morph (lift)
    import Control.Monad (when)

    import Data.Aeson
    import Data.Hashable
    import qualified Network.HTTP.Req as R
    import Data.ByteString.Char8 as B (unpack)
    import Data.ByteString.Lazy as LBS
    import Data.Time.Clock.POSIX

    import Network.Discord.Types as Dc
    import Network.Discord.Rest.Prelude

    data UserRequest a where
      GetCurrentUser       :: UserRequest User
      GetUser              :: Snowflake -> UserRequest User
      ModifyCurrentUser    :: ToJSON a => a -> UserRequest User
      GetCurrentUserGuilds :: Range -> UserRequest Guild
      LeaveGuild           :: Snowflake -> UserRequest ()
      GetUserDMs           :: UserRequest [Channel]
      CreateDM             :: Snowflake -> UserRequest Channel

    instance Hashable (UserRequest a) where
      hashWithSalt s (GetCurrentUser)         = hashWithSalt s  ("me"::Text)
      hashWithSalt s (GetUser _)              = hashWithSalt s  ("user"::Text)
      hashWithSalt s (ModifyCurrentUser _)    = hashWithSalt s  ("modify_user"::Text)
      hashWithSalt s (GetCurrentUserGuilds _) = hashWithSalt s  ("get_user_guilds"::Text)
      hashWithSalt s (LeaveGuild g)           = hashWithSalt s  ("leaveGuild"::Text, g)
      hashWithSalt s (GetUserDMs)             = hashWithSalt s  ("get_dms"::Text)
      hashWithSalt s (CreateDM _)             = hashWithSalt s  ("make_dm"::Text)

    instance Eq (UserRequest a) where
      a == b = hash a == hash b

    instance RateLimit (UserRequest a) where
      getRateLimit req = do
        st@DiscordState {getRateLimits=rl} <- ST.get
        now <- ST.liftIO (fmap round getPOSIXTime :: IO Int)
        case lookup (hash req) rl of
          Nothing -> return Nothing
          Just a
            | a >= now  -> return $ Just a
            | otherwise -> ST.put st{getRateLimits=Dc.delete (hash req) rl} >> return Nothing

      setRateLimit req reset = do
        st@DiscordState {getRateLimits=rl} <- ST.get
        ST.put st {getRateLimits=Dc.insert (hash req) reset rl}

    instance (FromJSON a) => DoFetch (UserRequest a) where
      doFetch req = do
        waitRateLimit req
        SyncFetched <$> fetch req

    fetch :: FromJSON a => UserRequest a -> DiscordM a
    fetch request = do
      opts <- baseRequestOptions
      let makeUrl c = baseUrl R./: "users" R./~ (T.pack c)
      let get c = (R.req R.GET (makeUrl c) R.NoReqBody R.lbsResponse opts) :: IO R.LbsResponse
      (resp, rlRem, rlNext) <- lift $ do
        resp :: R.LbsResponse <- case request of

          GetCurrentUser -> get "@me"

          GetUser user -> get user

          ModifyCurrentUser patch -> R.req R.PATCH (makeUrl "@me") (R.ReqBodyJson patch) R.lbsResponse opts

          GetCurrentUserGuilds range -> get $ "@me/guilds?" ++ toQueryString range

          LeaveGuild guild -> R.req R.DELETE (makeUrl $ "@me/guilds/" ++ guild) R.NoReqBody R.lbsResponse opts

          GetUserDMs -> get "@me/channels"

          CreateDM user -> let payload = object ["recipient_id" .= user]
                           in R.req R.POST (makeUrl $ "@me/channels") (R.ReqBodyJson payload) R.lbsResponse opts

        let parseIntFrom header = read $ B.unpack $ fromMaybe "0" $ R.responseHeader resp header -- FIXME: default int value
            -- justRight . eitherDecodeStrict $
        return (justRight $ eitherDecode $ (R.responseBody resp :: LBS.ByteString),
                parseIntFrom "X-RateLimit-Remaining", parseIntFrom "X-RateLimit-Reset")
      when (rlRem == 0) $ setRateLimit request rlNext
      return resp

