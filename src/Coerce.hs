{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}
module Coerce where
import Data.Functor.Product             -- Product of Functors
import Data.Functor.Compose             -- Composition of Functors
import Data.Functor.Const               -- Const Functor
import Control.Applicative              -- WrappedMonad
import Control.Monad.State.Lazy         -- State Monad
  
-- | This module provides explicition coercion.
--   Just in case you want to know what's behind the "magic" Data.Coerce.coerce 
class Coerce a b | a -> b where
    down :: a -> b
    up   :: b -> a

instance Coerce (Const a b) a where
    down = getConst
    up   = Const

instance (Coerce(m a) b, Coerce(n a) c) => Coerce((Product m n) a) (b,c) where
    down mnx = (down (pfst mnx), down(psnd mnx)) where
        pfst (Pair fst _) = fst
        psnd (Pair _ snd) = snd
    up (x,y) = Pair (up x) (up y)

instance (Functor m, Functor n, Coerce(m b)c, Coerce(n a)b) => Coerce((Compose m n) a) c where
    down = down . fmap down . getCompose
    up   = Compose . fmap up . up    

instance Coerce (m a) c => Coerce (WrappedMonad m a) c where
    down = down . unwrapMonad
    up   = WrapMonad . up

instance Coerce (State s a) (s -> (a,s)) where
    down = runState
    up   = state
    