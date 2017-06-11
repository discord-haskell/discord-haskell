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
  import Data.Hashable
  import Network.WebSockets (Connection)

  class (DiscordAuth m, MonadReader Connection m) => DiscordM m
  
  newtype DiscordApp m a = DiscordApp 
    { runEvent :: DiscordM m => Event -> m a }

  instance Alternative (DiscordApp m) where
    empty = DiscordApp $ const empty
    DiscordApp f <|> DiscordApp g = DiscordApp (\e -> f e <|> g e)

  instance Applicative (DiscordApp m) where
    pure a = DiscordApp (\_ -> return a)
    DiscordApp f <*> DiscordApp a =
      DiscordApp (\e -> f e <*> a e)

  instance DiscordAuth (DiscordApp m) where
    auth    = DiscordApp $ const auth
    version = DiscordApp $ const version
    runIO   = fail "DiscordApp cannot be lifted to IO"

  rateLimits :: Vault (DiscordApp m) [(Int, Int)]
  rateLimits = unsafePerformIO $ newMVar []
  {-# NOINLINE rateLimits #-}

  delete :: Eq a => [(a, b)] -> a -> [(a, b)]
  delete ((a, b):xs) a'
    | a == a'   = delete xs a'
    | otherwise = (a, b):delete xs a'
  delete [] _ = []

  modify :: Eq a => a -> b -> [(a, b)] -> [(a, b)]
  modify a' b' ((a, b):xs)
    | a == a'   = (a', b'): delete xs a
    | otherwise = (a, b): modify a' b' xs
  modify a' b' [] = [(a', b')]

  instance DiscordM m => DiscordRest (DiscordApp m) where
    getRateLimit f = lookup (hash f) =<< get rateLimits
      where
        lookup :: (Eq a, Monad m) => a -> [(a, b)] -> m (Maybe b)
        lookup a' ((a, b):xs)
          | a' == a   = return (Just b)
          | otherwise = lookup a' xs
        lookup _ [] = return Nothing
    setRateLimit f limit = put rateLimits =<< modify (hash f) limit `fmap` get rateLimits

  instance DiscordM m => DiscordGate (DiscordApp m) where
    type Vault (DiscordApp m) = MVar

    data VaultKey (DiscordApp m) a = Store (MVar a)
    get = liftIO . readMVar
    put s v = liftIO $ putMVar s v

    sequenceKey = Store $ unsafePerformIO newEmptyMVar
    {-# NOINLINE sequenceKey #-}
    storeFor (Store var) = return var

    connection = DiscordApp $ const ask
    feed m event = do
      c <- connection
      _ <- liftIO . forkIO . runIO $ local (const c) (runEvent m event)
      return ()

    run m conn =
      runIO $ local (const conn) ((runEvent $ eventStream Create m) Nil)
    fork m = do
      c <- connection
      _ <- DiscordApp $ \e -> liftIO . forkIO . runIO $ local (const c) (runEvent m e)
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

  data a :> b
  data a :<>: b

  instance (DiscordRest m, EventMap f m, EventMap g m, Codomain f ~ Domain g)
     => EventMap (f :> g) m where

     type Domain   (f :> g) = Domain f
     type Codomain (f :> g) = Codomain g

     mapEvent p event = mapEvent b =<< mapEvent a event
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

