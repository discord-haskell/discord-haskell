{-# LANGUAGE GADTs, OverloadedStrings, InstanceSigs, TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
-- | Provide actions for User API interactions.
module Network.Discord.Rest.User
  (
    UserRequest(..)
  ) where
    
    import Data.Aeson
    import Data.Hashable
    import Data.Monoid (mempty)
    import Data.Text as T
    
    import Network.Discord.Rest.Prelude
    import Network.Discord.Types
    import Network.Discord.Rest.HTTP

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
      hashWithSalt s (LeaveGuild g)           = hashWithSalt s  ("leave_guild"::Text, g)
      hashWithSalt s (GetUserDMs)             = hashWithSalt s  ("get_dms"::Text)
      hashWithSalt s (CreateDM _)             = hashWithSalt s  ("make_dm"::Text)

    instance RateLimit (UserRequest a)

    instance (FromJSON a) => DoFetch (UserRequest a) where
      doFetch req = SyncFetched <$> go req
        where
          url = baseUrl /: "users"
          go :: UserRequest a -> DiscordM a
          go r@(GetCurrentUser) = makeRequest r
            $ Get (url /: "@me") mempty

          go r@(GetUser user) = makeRequest r 
            $ Get (url // user ) mempty

          go r@(ModifyCurrentUser patch) = makeRequest r 
            $ Patch (url /: "@me")  (ReqBodyJson patch) mempty

          go r@(GetCurrentUserGuilds range) = makeRequest r 
            $ Get url $ toQueryString range

          go r@(LeaveGuild guild) = makeRequest r
            $ Delete (url /: "@me" /: "guilds" // guild) mempty

          go r@(GetUserDMs) = makeRequest r
            $ Get (url /: "@me" /: "channels") mempty

          go r@(CreateDM user) = makeRequest r
            $ Post (url /: "@me" /: "channels")
            (ReqBodyJson $ object ["recipient_id" .= user])
            mempty
