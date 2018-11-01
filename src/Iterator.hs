{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}
module Iterator where
import Singleton (Exp (..))
import Visitor

import Data.Functor.Product
import Data.Functor.Compose
import Data.Functor.Const               -- for Const
import Data.Monoid (Sum (..), getSum)   -- for Sum
import Control.Monad.State.Lazy         -- for State
import Control.Applicative              -- for WrappedMonad

instance Functor Exp where
    fmap f (Var x)       = Var x
    fmap f (Val a)       = Val $ f a
    fmap f (Add x y)     = Add (fmap f x) (fmap f y)
    fmap f (Mul x y)     = Mul (fmap f x) (fmap f y)

instance Traversable Exp where
    traverse g (Var x)   = pure $ Var x
    traverse g (Val x)   = Val <$> g x
    traverse g (Add x y) = Add <$> traverse g x <*> traverse g y
    traverse g (Mul x y) = Mul <$> traverse g x <*> traverse g y

-- Functor Product
(<#>) :: (Functor m, Functor n) => (a -> m b) -> (a -> n b) -> (a -> Product m n b)
(f <#> g) y = Pair (f y) (g y) 

-- Functor composition
(<.>) :: (Functor m, Functor n) => (b -> n c) -> (a -> m b) -> (a -> (Compose m n) c)
f <.> g = Compose . fmap f . g

--{--
class Coerce a b | a -> b where
    down :: a -> b
    up   :: b -> a

instance Coerce (Const a b) a where
    down = getConst
    up   = Const

instance (Coerce(m a) b, Coerce(n a) c) => Coerce((Product m n) a) (b,c) where
    down mnx = (down (pfst mnx), down(psnd mnx))
    up (x,y) = Pair (up x) (up y)

instance (Functor m, Functor n, Coerce(m b)c, Coerce(n a)b) => Coerce((Compose m n) a) c where
    down = down . fmap down . getCompose
    up   = Compose . fmap up . up    

instance Coerce (m a) c => Coerce (WrappedMonad m a) c where
    down = down . unwrapMonad
    up   = WrapMonad . up

instance Coerce (State s a) (s -> (a,s)) where
    down = runState
    up = state
--}
type Count = Const (Sum Integer)

count :: a -> Count b
count _ = Const 1

cciBody :: Char -> Count a
cciBody = count

cci :: String -> Count [a]
cci = traverse cciBody


lciBody :: Char -> Count a
lciBody c = Const $ test (c == '\n')

test :: Bool -> Sum Integer
test b = Sum $ if b then 1 else 0

lci :: String -> Count [a]
lci = traverse lciBody

clci :: String -> Product Count Count [a]
clci = traverse (cciBody <#> lciBody)

wciBody :: Char -> Compose (WrappedMonad (State Bool)) Count a
wciBody c =  up (updateState c) where
    updateState :: Char -> Bool -> (Sum Integer, Bool)
    updateState c w = let s = not(isSpace c) in (test (not w && s), s)
    isSpace :: Char -> Bool
    isSpace c = c == ' ' || c == '\n' || c == '\t'

wci :: String -> Compose (WrappedMonad (State Bool)) Count [a]
wci = traverse wciBody

clwci :: String -> (Product (Product Count Count) (Compose (WrappedMonad (State Bool)) Count)) [a]
clwci = traverse (cciBody <#> lciBody <#> wciBody)

str :: String
str = "hello nice \n and busy world"

pfst :: Product f g a -> f a
pfst (Pair fst _) = fst
psnd :: Product f g a -> g a
psnd (Pair _ snd) = snd

iteratorDemo = do
    putStrLn "Iterator -> Traversable"
    let exp = Mul (Add (Val 3) (Val 1)) 
                (Mul (Val 2) (Var "pi"))
        env = [("pi", pi)]
    print $ traverse (\x c -> if even x then [x] else [2*x]) exp 0
    let wordcount = clwci str 
    print $ getSum $ getConst $ pfst (pfst wordcount) 
    print $ getSum $ getConst $ psnd (pfst wordcount)
    print $ getSum $ getConst $ evalState (unwrapMonad (getCompose (psnd wordcount))) False
                        
    