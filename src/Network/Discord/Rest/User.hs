{-# LANGUAGE GADTs, OverloadedStrings, InstanceSigs, TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, ScopedTypeVariables #-}
-- | Provide actions for User API interactions.
module Network.Discord.Rest.User
  (
    UserRequest(..)
  ) where
    import Control.Monad (when)
    import Data.Maybe (fromMaybe)

    import Control.Concurrent.STM
    import Control.Monad.Morph (lift)
    import Control.Lens
    import Data.Aeson
    import Data.Hashable

    import qualified Network.HTTP.Req as R
    import Data.ByteString.Char8 as B (unpack)
    import Data.ByteString.Lazy as LBS
    import Data.Text as T

    import Data.Time.Clock.POSIX
    import qualified Control.Monad.State as ST (get, liftIO)

    import Network.Discord.Rest.Prelude
    import Network.Discord.Types as Dc

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
      opts <- baseRequestOptions
      let makeUrl c = baseUrl R./: "users" R./~ (T.pack c)
      let get c = (R.req R.GET (makeUrl c) R.NoReqBody R.lbsResponse opts) :: IO R.LbsResponse
      (resp, rlRem, rlNext) <- lift $ do
        resp :: R.LbsResponse <- case request of

          GetCurrentUser -> get "@me"

          GetUser user -> get user

          ModifyCurrentUser patch -> R.req R.PATCH (makeUrl "@me")
                                           (R.ReqBodyJson patch) R.lbsResponse opts

          GetCurrentUserGuilds range -> get $ "@me/guilds?" ++ toQueryString range

          LeaveGuild guild -> R.req R.DELETE (makeUrl $ "@me/guilds/" ++ guild)
                                    R.NoReqBody R.lbsResponse opts

          GetUserDMs -> get "@me/channels"

          CreateDM (Snowflake user) -> let payload = object ["recipient_id" .= user]
                                       in R.req R.POST (makeUrl $ "@me/channels")
                                            (R.ReqBodyJson payload) R.lbsResponse opts

        let parseIntFrom header = read $ B.unpack $ fromMaybe "0" $ R.responseHeader resp header -- FIXME: default int value
            -- justRight . eitherDecodeStrict $
        return (justRight $ eitherDecode $ (R.responseBody resp :: LBS.ByteString),
                parseIntFrom "X-RateLimit-Remaining", parseIntFrom "X-RateLimit-Reset")
      when (rlRem == 0) $ setRateLimit request rlNext
      return resp

