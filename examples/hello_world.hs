{-# LANGUAGE OverloadedStrings, TypeOperators, FlexibleInstances, MultiParamTypeClasses, TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

import Data.Proxy

import Network.Discord

instance DiscordAuth IO where
  auth    = return "TOKEN"
  version = "0.2.2"
  runIO = id

data HelloWorld

instance EventMap HelloWorld (DiscordApp IO) where
  type Domain   HelloWorld = Init
  type Codomain HelloWorld = ()

  mapEvent _ _ = do
    _ <- doFetch $ CreateMessage "188134500411244545" "Hello, World!"
    return ()

type HelloWorldApp = ReadyEvent :> HelloWorld

instance EventHandler HelloWorldApp (DiscordApp IO)

main :: IO ()
main = runBot (Proxy :: Proxy (DiscordApp IO HelloWorldApp))
