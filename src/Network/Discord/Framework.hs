-- | Provides a convenience framework for writing Discord bots without dealing with Pipes
{-# LANGUAGE TypeOperators, RankNTypes, TypeFamilies, MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}
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
  import Network.URL (URL)

  class (DiscordRest m, MonadReader Connection m) => DiscordM m
  
  newtype DiscordApp m a = DiscordApp 
    { runEvent :: DiscordM m => Event -> m a }

  instance Alternative (DiscordApp m) where
    empty = DiscordApp (\_ -> empty)
    DiscordApp f <|> DiscordApp g = DiscordApp (\e -> f e <|> g e)

  instance Applicative (DiscordApp m) where
    pure a = DiscordApp (\_ -> return a)
    DiscordApp f <*> DiscordApp a =
      DiscordApp (\e -> (f e) <*> (a e))

  instance DiscordAuth (DiscordApp m) where
    auth    = DiscordApp (\_ -> auth)
    version = DiscordApp (\_ -> version)
    runIO   = fail "DiscordApp cannot be lifted to IO"

  instance DiscordM m => DiscordGate (DiscordApp m) where
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
      _ <- liftIO . forkIO . runIO $ local (\_ -> c) (runEvent m event)
      return ()

    run m conn =
      runIO $ local (\_ -> conn) ((runEvent $ eventStream Create m) Nil)
    fork m = do
      c <- connection
      _ <- DiscordApp $ \e -> liftIO . forkIO . runIO $ local (\_ -> c) ((runEvent m) e)
      return ()

  instance Functor (DiscordApp m) where
    f `fmap` DiscordApp a = DiscordApp (\e -> f `fmap` a e)

  instance Monad (DiscordApp m) where
    m >>= k = DiscordApp $ \e -> do
      a <- runEvent m e
      runEvent (k a) e

  instance MonadIO (DiscordApp m) where
    liftIO f = DiscordApp (\_ -> liftIO f)

  instance MonadPlus (DiscordApp m)

  class DiscordRest m => EventMap f m where
    type Domain f
    type Codomain f
    mapEvent :: Proxy f -> Domain f -> m (Codomain f)

  class (DiscordM m, Event ~ Domain f,  () ~ Codomain f, EventMap f m) => EventHandler f m

  data a :> b
  data a :<>: b

  instance (DiscordRest m, EventMap f m, EventMap g m, Codomain f ~ Domain g)
     => EventMap (f :> g) m where

     type Domain   (f :> g) = Domain f
     type Codomain (f :> g) = Codomain g

     mapEvent p event = mapEvent b =<< (mapEvent a event)
      where
        (a, b) = split p
        split :: Proxy (a :> b) -> (Proxy a, Proxy b)
        split _ = (Proxy, Proxy)

  instance (DiscordRest m, EventMap f m, EventMap g m
    , Domain f ~ Domain g, Codomain f ~ Codomain g)
    => EventMap (f :<>: g) m where
    
    type Domain   (f :<>: g) = Domain f
    type Codomain (f :<>: g) = Codomain f

    mapEvent p event = mapEvent a event <|> mapEvent b event
      where
        (a, b) = split p
        split :: Proxy (a :<>: b) -> (Proxy a, Proxy b)
        split _ = (Proxy, Proxy)

  runBot :: (DiscordM m, EventHandler f m) => Proxy (m f) -> URL -> IO ()
  runBot p url = runGateway url (DiscordApp $ go p)
    where
      split :: Proxy (a b) -> (Proxy a, Proxy b)
      split _ = (Proxy, Proxy)
      go :: EventHandler f m => Proxy (m f) -> Event -> m ()
      go p' = let (_, b) = split p' in mapEvent b
