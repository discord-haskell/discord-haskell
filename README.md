# Discord.hs
A Haskell wrapper for the Discord API

## Installing
Discord-hs isn't on hackage or stackage (Yet, I'll upload when documentation is in a good state.)  
In order to install:
```sh
git clone https://github.com/jano017/Discord.hs.git
cd discord.hs
cabal install
```
Stack support to come.

## PingPong
```haskell
{-# LANGUAGE OverloadedStrings #-}
import Data.Text

import Network.Discord

data PingPongClient = PingPongClient
instance Client PingPongClient where
  getAuth _ = "TOKEN"

main :: IO ()
main = do
  gateway <- getGateway
  runWebsocket gateway PingPongClient $ do
    DiscordState {getWebSocket=ws} <- get
    (eventCore ~> \event -> case event of
      Ready (Init v u _ _ _) -> liftIO . putStrLn $ "Connected to gateway v"++show v
        ++ " as user " ++ show u
      MessageCreate (Message {messageChannel=chan, messageAuthor=user, messageContent=cont}) ->
        when ("Ping" `isPrefixOf` cont && not (userIsBot user)) $
          void $ restServer +>> fetch (CreateMessage chan "Pong!")
      _ -> return ()
      ) ws
```

## Core components:
- `eventCore :: Connection -> Producer Event DiscordM ()`
  - Converts a websocket `Connection` to a `Producer` yielding `Event`s (Network.Discord.Gateway)
- `DiscordState { getWebsocket :: Connection, getClient :: Client a => a }`
  - Stores state information (Client, Websocket Connection, ect.) Initialized in runWebsocket. (Network.Discord.Types)
- `restServer :: Fetchable -> Server Fetchable Fetched DiscordM Fetched`
  - Runs `Fetchable`s according to `DoFetch` implementations. Used for REST api interactions.
- `fetch :: DoFetch a -> a -> Client Fetchable Fetched DiscordM Fetched`
  - Pass a `Fetchable` upstream to a runServer. Multiple `fetch`es can be chained to a single `runServer` using
    either `do` notation or `(>>)`

## Known issues:
- Init isn't parsing correctly
- Client doesn't close correctly
- All json serialization is currently in Types.Channel. Should be broken out
- Only Channel api endpoints are implemented
- Missing voice support
- Missing stack support
- Travis-CI build fails

## Future goals:
- [Eta](https://github.com/typelead/eta) compatibility
- [HaLVM](https://github.com/GaloisInc/HaLVM) compatibility (maybe)
- Tighten properties/prove properties
- Monad based declarative bots
- Proper logging?
