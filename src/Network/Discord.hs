-- | Provides core Discord functionallity. 
{-# LANGUAGE DataKinds, TypeOperators, KindSignatures, RankNTypes, FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses, GADTs, TypeFamilies, FlexibleContexts #-}
module Network.Discord
  ( module Network.Discord.Framework
  , module Network.Discord.Rest
  , module Network.Discord.Types
  , module Network.Discord.Gateway
  ) where
  import Network.Discord.Framework
  import Network.Discord.Rest
  import Network.Discord.Types
  import Network.Discord.Gateway

  import Data.Proxy
  import Data.Type.Equality
  import GHC.TypeLits hiding ((:<>:))

  data a :> b
  infixr 5 :> 

  data a :<>: b = a :<>: b
  infixl 3 :<>:

  instance (HasEvent a m e, HasEvent b m e) => HasEvent (a :<>: b) m e where
    type EventHandler' (a :<>: b) m = EventHandler' a m :<>: EventHandler' b m
    makeApp p = Choose (makeApp a) (makeApp b)
      where
        (a, b) = split p
        split :: Proxy (a :<>: b) -> (Proxy a, Proxy b)
        split _ = (Proxy, Proxy)
    stepApp p (Choose a b) (f :<>: g) event = do
      stepApp pa a f event
      stepApp pb b g event
      where
        (pa, pb) = split p
        split :: Proxy (a :<>: b) -> (Proxy a, Proxy b)
        split _ = (Proxy, Proxy)
    stepApp _ _ _ _ = return ()

  data EventHandle a

  instance DiscordRest m => HasEvent (EventHandle a) m a where
    type EventHandler' (EventHandle a) m = a -> m ()
    makeApp p = leaf p
      where
        leaf :: Proxy (EventHandle a) -> DiscordApp' m a
        leaf _ = Leaf
    stepApp _ Leaf f event = f event
    stepApp _ _ _ _ = return ()

  class DiscordRest m => EventMap f m where
    type Domain f
    type Codomain f
    mapEvent :: Proxy f -> Domain f -> m (Codomain f)
  
  instance (a ~ Domain f, EventMap f m, HasEvent g m (Codomain f)) 
    => HasEvent (f :> g) m a where
    type EventHandler' (f :> g) m = EventHandler' g m
    makeApp p = Transform (mapEvent a) (makeApp b)
      where
        (a, b) = split p
        split :: Proxy (a :> b) -> (Proxy a, Proxy b)
        split _ = (Proxy, Proxy)
    stepApp p (Transform f api) app e = f e >>=
      stepApp b api app
      where
        (a, b) = split p
        split :: Proxy (a :> b) -> (Proxy a, Proxy b)
        split _ = (Proxy, Proxy)
