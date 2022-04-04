### Design

```haskell
~> :info runDiscord
runDiscord :: RunDiscordOpts -> IO T.Text {- Text is user facing error. Print it -}

~> :info RunDiscordOpts
data RunDiscordOpts = RunDiscordOpts
    { discordToken :: T.Text
    , discordOnStart :: DiscordHandler ()
    , discordOnEnd :: IO ()
    , discordOnEvent :: Event -> DiscordHandler ()  -- response to action
    , discordOnLog :: Text -> IO ()
    , discordForkThreadForEvents :: Bool
    }

~> :info DiscordHandler
type DiscordHandler = ReaderT DiscordHandle IO

{- ReaderT for access to the Handle -}

{- Event handlers and Options for the program

  An exception in discordOnStart exits the program immediately
  An exception in discordOnEvent continues loop

-}

 ~> :info DiscordHandle
data DiscordHandle = DiscordHandle
    { --[[ Some internal data that you normally won't use ]]
      --[[ Makes sure we don't violate rate-limits! ]]
      --[[ Threadsafe access ]]
    }

{- Import Discord.Handle to view the insides -}

```


#### Internals

Use `Chan`s to pass data between threads.

#### Websocket loop

Make user handle events as they happen

#### Rest request loop

Allow executing rest requests without overstepping ratelimits
