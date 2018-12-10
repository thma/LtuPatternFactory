{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE UndecidableInstances   #-}

module Coerce where

import           Control.Applicative
import           Control.Monad.State.Lazy
import           Data.Functor.Compose
import           Data.Functor.Const
import           Data.Functor.Product
import           Data.Monoid              (Sum (..), getSum)
import           Data.Typeable

-- | This module provides explicit coercion.
--   Instead of the "magic" Data.Coerce.coerce you could use wrap and unwrap to explicitly write the coercions.
class Coerce a b | a -> b where
  unwrap :: a -> b
  wrap :: b -> a

instance Coerce (Const a b) a where
  unwrap = getConst
  wrap = Const

instance Coerce (Sum a) a where
  unwrap = getSum
  wrap = Sum

instance (Coerce (m a) b, Coerce (n a) c) =>
         Coerce ((Product m n) a) (b, c) where
  unwrap mnx = (unwrap (pfst mnx), unwrap (psnd mnx))
    where
      pfst (Pair fst _) = fst
      psnd (Pair _ snd) = snd
  wrap (x, y) = Pair (wrap x) (wrap y)

instance (Functor m, Functor n, Coerce (m b) c, Coerce (n a) b) =>
         Coerce ((Compose m n) a) c where
  unwrap = unwrap . fmap unwrap . getCompose
  wrap = Compose . fmap wrap . wrap

instance Coerce (m a) c => Coerce (WrappedMonad m a) c where
  unwrap = unwrap . unwrapMonad
  wrap = WrapMonad . wrap

instance Coerce (State s a) (s -> (a, s)) where
  unwrap = runState
  wrap = state
