{-# LANGUAGE OverloadedStrings, TypeOperators, FlexibleInstances, MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds, TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

import Control.Monad (mzero)
import Data.Proxy
import GHC.TypeLits

import Network.Discord

import Control.Monad.IO.Class

instance DiscordAuth IO where
  auth    = return $ Bot "TOKEN"
  version = return "0.2.2"
  runIO   = id

data Command (a :: Symbol)

instance KnownSymbol a => EventMap (Command a) (DiscordApp IO) where
  type Domain   (Command a) = Message
  type Codomain (Command a) = Message

  mapEvent p (m@Message{messageContent = c})
    | show c == symbolVal (commandName p) = return m
    | otherwise = mzero
    where
      commandName :: Proxy (Command a) -> Proxy a
      commandName _ = Proxy

data Reply (a :: Symbol)

instance KnownSymbol a => EventMap (Reply a) (DiscordApp IO) where
  type Domain   (Reply a) = Message
  type Codomain (Reply a) = ()

  mapEvent p (Message{messageChannel = c}) = do
    _ <- doFetch $ CreateMessage c (read . symbolVal $ replyText p) Nothing
    return ()
    where
      replyText :: Proxy (Reply a) -> Proxy a
      replyText _ = Proxy

data LogEvent a

instance Show a => EventMap (LogEvent a) (DiscordApp IO) where
  type Domain   (LogEvent a) = a
  type Codomain (LogEvent a) = ()

  mapEvent _ e = do
    liftIO $ putStrLn "Logging Event"
    liftIO . putStrLn $ show e

type PingPongApp = LogEvent Event :<>: ((MessageCreateEvent :<>: MessageUpdateEvent) :> 
  (    (Command "ping" :> Reply "pong")
  :<>: (Command "pong" :> Reply "ping")
  ))

instance EventHandler PingPongApp IO

main :: IO ()
main = runBot (Proxy :: Proxy (IO PingPongApp))
