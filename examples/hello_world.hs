{-# LANGUAGE OverloadedStrings, TypeOperators, FlexibleInstances, MultiParamTypeClasses, TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

import Data.Proxy

import Network.Discord

import Control.Monad.IO.Class

instance DiscordAuth IO where
  auth    = return $ Bot "TOKEN"
  version = return "0.2.2"
  runIO   = id

data HelloWorld

instance EventMap HelloWorld (DiscordApp IO) where
  type Domain   HelloWorld = Init
  type Codomain HelloWorld = ()

  mapEvent _ _ = liftIO $ putStrLn "Hello, world!"

type HelloWorldApp = ReadyEvent :> HelloWorld

instance EventHandler HelloWorldApp IO

main :: IO ()
main = runBot (Proxy :: Proxy (IO HelloWorldApp))
