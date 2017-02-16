{-# LANGUAGE GADTs, OverloadedStrings, InstanceSigs, TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
-- | Provide actions for User API interactions.
module Network.Discord.Rest.User
  (
    UserRequest(..)
  ) where
    import Data.Text
    import qualified Control.Monad.State as ST (get, liftIO)
    import Control.Monad.Morph (lift)
    import Control.Monad (when)
    import Control.Concurrent.STM

    import Data.Aeson
    import Data.Hashable
    import Network.Wreq
    import Control.Lens
    import Data.Time.Clock.POSIX

    import Network.Discord.Types as Dc
    import Network.Discord.Rest.Prelude

    -- | Data constructor for User requests. See
    --   <https://discordapp.com/developers/docs/resources/user User API>
    data UserRequest a where
      -- | Returns the 'User' object of the requester's account. For OAuth2, this requires
      --   the identify scope, which will return the object without an email, and optionally 
      --   the email scope, which returns the object with an email.
      GetCurrentUser       :: UserRequest User
      -- | Returns a 'User' for a given user ID
      GetUser              :: Snowflake -> UserRequest User
      -- | Modify the requestors user account settings. Returns a 'User' object on success.
      ModifyCurrentUser    :: ToJSON a => a -> UserRequest User
      -- | Returns a list of user 'Guild' objects the current user is a member of.
      --   Requires the guilds OAuth2 scope.
      GetCurrentUserGuilds :: Range -> UserRequest Guild
      -- | Leave a guild.
      LeaveGuild           :: Snowflake -> UserRequest ()
      -- | Returns a list of DM 'Channel' objects
      GetUserDMs           :: UserRequest [Channel]
      -- | Create a new DM channel with a user. Returns a DM 'Channel' object.
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
        DiscordState {getRateLimits=rl} <- ST.get
        now <- ST.liftIO (fmap round getPOSIXTime :: IO Int)
        ST.liftIO . atomically $ do
          rateLimits <- readTVar rl
          case lookup (hash req) rateLimits of
            Nothing -> return Nothing
            Just a
              | a >= now  -> return $ Just a
              | otherwise -> modifyTVar' rl (Dc.delete $ hash req) >> return Nothing

      setRateLimit req reset = do
        DiscordState {getRateLimits=rl} <- ST.get
        ST.liftIO . atomically . modifyTVar rl $ Dc.insert (hash req) reset

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
            ("/users/" ++ show user)
          ModifyCurrentUser patch -> customPayloadMethodWith "PATCH" req
            "/users/@me"
            (toJSON patch)
          GetCurrentUserGuilds range -> getWith req
            ("/users/@me/guilds?" ++ toQueryString range)
          LeaveGuild guild -> deleteWith req
            ("/users/@me/guilds/" ++ show guild)
          GetUserDMs -> getWith req
            "/users/@me/channels"
          CreateDM (Snowflake user) -> postWith req
            "/users/@me/channels"
            ["recipient_id" := user]
        return (justRight . eitherDecode $ resp ^. responseBody
          , justRight . eitherDecodeStrict $ resp ^. responseHeader "X-RateLimit-Remaining"::Int
          , justRight . eitherDecodeStrict $ resp ^. responseHeader "X-RateLimit-Reset"::Int)
      when (rlRem == 0) $ setRateLimit request rlNext
      return resp
