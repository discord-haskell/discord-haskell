-- | Provides a convenience framework for writing Discord bots without dealing with Pipes
{-# LANGUAGE DataKinds, TypeOperators, KindSignatures, RankNTypes, FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses, GADTs, TypeFamilies, FlexibleContexts #-}
module Network.Discord.Framework where
  import Data.Proxy
  import Control.Applicative
  import Control.Concurrent
  import System.IO.Unsafe (unsafePerformIO)

  import Network.Discord.Rest
  import Network.Discord.Gateway
  import Network.Discord.Types

  import Control.Monad.Reader
  import Network.WebSockets (Connection)
  
  newtype DiscordApp m a = DiscordApp 
    { runApp :: DiscordRest m => Event -> ReaderT Connection m a }

  instance Alternative (DiscordApp m) where
    empty = DiscordApp (\_ -> empty)
    DiscordApp f <|> DiscordApp g = DiscordApp (\e -> f e <|> g e)

  instance Applicative (DiscordApp m) where
    pure a = DiscordApp (\_ -> return a)
    DiscordApp f <*> DiscordApp a =
      DiscordApp (\e -> (f e) <*> (a e))

  instance DiscordAuth (DiscordApp m) where
    auth    = DiscordApp (\_ -> lift auth)
    version = DiscordApp (\_ -> lift version)
    runIO   = fail "DiscordApp cannot be lifted to IO"

  instance DiscordRest m => DiscordGate (DiscordApp m) where
    type Vault (DiscordApp m) = MVar

    data VaultKey (DiscordApp m) a = Store (MVar a)
    get = liftIO . readMVar
    put s v = liftIO $ putMVar s v

    sequenceKey = Store $ unsafePerformIO newEmptyMVar
    {-# NOINLINE sequenceKey #-}
    storeFor (Store var) = return var

    connection = DiscordApp (\_ -> ask)
    feed m event = do
      c <- connection
      _ <- liftIO . forkIO . runIO $ runReaderT ((runApp m) event) c
      return ()

    run m conn =
      runIO $ runReaderT ((runApp $ eventStream Create m) Nil) conn
    fork m = do
      c <- connection
      _ <- DiscordApp $ \e -> liftIO . forkIO . runIO $ runReaderT ((runApp m) e) c
      return ()

  instance Functor (DiscordApp m) where
    f `fmap` DiscordApp a = DiscordApp (\e -> f `fmap` a e)

  instance Monad (DiscordApp m) where
    m >>= k = DiscordApp $ \e -> do
      a <- runApp m e
      runApp (k a) e

  instance MonadIO (DiscordApp m) where
    liftIO f = DiscordApp (\_ -> liftIO f)

  instance MonadPlus (DiscordApp m)

  class (DiscordRest m, HasEvent api m Event)
    => EventHandler api app m

  data DiscordApp' m a where
    Leaf      :: DiscordApp' m a
    Transform :: (a -> m b)    -> DiscordApp' m b -> DiscordApp' m a
    Choose    :: DiscordApp' m a -> DiscordApp' m a -> DiscordApp' m a

  class DiscordRest m => HasEvent api m event where
    type EventHandler' api m
    makeApp :: Proxy api -> DiscordApp' m event
    stepApp :: Proxy api -> DiscordApp' m event -> EventHandler' api m -> event -> m ()
