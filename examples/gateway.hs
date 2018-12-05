{-# LANGUAGE OverloadedStrings #-}

import Control.Concurrent (forkIO, threadDelay)
import Control.Exception (finally)
import Data.Monoid ((<>))
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Discord

-- | Prints every event as it happens
gatewayExample :: IO ()
gatewayExample = do
  tok <- T.strip <$> TIO.readFile "./examples/auth-token.secret"
  dis <- loginRestGateway (Auth tok)

  _ <- forkIO $ do
    sendCommand dis (UpdateStatus (UpdateStatusOpts Nothing
                                    UpdateStatusAwayFromKeyboard True))
    threadDelay (3 * 10^6)
    sendCommand dis (UpdateStatus (UpdateStatusOpts Nothing
                                    UpdateStatusOnline False))

  finally (let loop = do
                  e <- nextEvent dis
                  case e of
                      Left er -> putStrLn ("Event error: " <> show er)
                      Right x -> do
                        putStrLn (show x <> "\n")
                        loop
           in loop)
          (stopDiscord dis)

