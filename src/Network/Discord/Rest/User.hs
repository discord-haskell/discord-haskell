{-# LANGUAGE GADTs, OverloadedStrings, InstanceSigs, TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
module Network.Discord.Rest.User
  (
    UserRequest(..)
  ) where
    import Data.Text
    import qualified Control.Monad.State as ST (get, put, liftIO)
    import Control.Monad.Morph (lift)
    import Control.Monad (when)

    import Data.Aeson
    import Data.Hashable
    import Network.Wreq
    import Control.Lens
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
      req <- baseRequest
      (resp, rlRem, rlNext) <- lift $ do
        resp <- case request of
          GetCurrentUser -> getWith req
            "/users/@me"
          GetUser user -> getWith req
            ("/users/"++user)
          ModifyCurrentUser patch -> customPayloadMethodWith "PATCH" req
            "/users/@me"
            (toJSON patch)
          GetCurrentUserGuilds range -> getWith req
            ("/users/@me/guilds?" ++ toQueryString range)
          LeaveGuild guild -> deleteWith req
            ("/users/@me/guilds/"++guild)
          GetUserDMs -> getWith req
            "/users/@me/channels"
          CreateDM user -> postWith req
            "/users/@me/channels"
            ["recipient_id" := user]
        return (justRight . eitherDecode $ resp ^. responseBody
          , justRight . eitherDecodeStrict $ resp ^. responseHeader "X-RateLimit-Remaining"::Int
          , justRight . eitherDecodeStrict $ resp ^. responseHeader "X-RateLimit-Reset"::Int)
      when (rlRem == 0) $ setRateLimit request rlNext
      return resp
