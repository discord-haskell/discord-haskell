{-# LANGUAGE OverloadedStrings, TypeOperators, FlexibleInstances, MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds, TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

import Control.Monad (mzero)
import Data.Proxy
import GHC.TypeLits

import Network.Discord

import Control.Monad.IO.Class
import Data.Text

instance DiscordAuth IO where
  auth    = return $ Bot "TOKEN"
  version = return "0.2.2"
  runIO   = id

data Command (a :: Symbol)

instance KnownSymbol a => EventMap (Command a) (DiscordApp IO) where
  type Domain   (Command a) = Message
  type Codomain (Command a) = Message

  mapEvent p (m@Message{messageContent = c,messageAuthor = User{userIsBot = bot}})
    | bot = do
      liftIO $ putStrLn "Ignoring Bot Message"
      mzero
    | unpack c == symbolVal (commandName p) = do
      liftIO $ putStrLn "Command Match"
      return m
    | otherwise = do
      liftIO . putStrLn $ "No Command Match: " ++ show c ++ "!=" ++ symbolVal (commandName p)
      mzero
    where
      commandName :: Proxy (Command a) -> Proxy a
      commandName _ = Proxy

data Reply (a :: Symbol)

instance KnownSymbol a => EventMap (Reply a) (DiscordApp IO) where
  type Domain   (Reply a) = Message
  type Codomain (Reply a) = ()

  mapEvent p (Message{messageChannel = c}) = do
    _ <- doFetch $ CreateMessage c (pack . symbolVal $ replyText p) Nothing
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

type PingPongApp = 
  (
    (MessageCreateEvent :<>: MessageUpdateEvent) :> 
      (    (Command "ping" :> Reply "pong")
      :<>: (Command "pong" :> Reply "ping")
      )
  ) :<>: LogEvent Event

instance EventHandler PingPongApp IO

main :: IO ()
main = runBot (Proxy :: Proxy (IO PingPongApp))
