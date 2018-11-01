{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, UndecidableInstances #-}
module Iterator where
import Singleton (Exp (..))
import Visitor
import Data.Functor.Const
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

-- the applicative product adventure

data Prod m n a = Prod {pfst:: m a, psnd:: n a} deriving (Show)

instance (Functor m, Functor n) => Functor (Prod m n) where    
    fmap f (Prod m n) = Prod (fmap f m) (fmap f n)

instance (Applicative m, Applicative n) => Applicative (Prod m n) where
    pure x    = Prod (pure x) (pure x)
    mf <*> mx = Prod (pfst mf <*> pfst mx) (psnd mf <*> psnd mx)

-- Functor Product
(<#>) :: (Functor m, Functor n) => (a -> m b) -> (a -> n b) -> (a -> Prod m n b)
(f <#> g) y = Prod (f y) (g y) 

newtype Comp m n a = Comp {unComp :: m (n a)} deriving (Show)

instance (Functor m, Functor n) => Functor (Comp m n) where  
    fmap f (Comp x) = Comp (fmap (fmap f) x)  

instance (Applicative m, Applicative n) => Applicative (Comp m n) where
    pure x                  = Comp (pure (pure x))
    (Comp mf) <*> (Comp mx) = Comp (pure (<*>) <*> mf <*> mx)

(<.>) :: (Functor m, Functor n) => (b -> n c) -> (a -> m b) -> (a -> (Comp m n) c)
f <.> g = Comp . fmap f . g

class Coerce a b | a -> b where
    down :: a -> b
    up   :: b -> a

instance Coerce (Const a b) a where
    down = getConst
    up   = Const

instance (Coerce(m a) b, Coerce(n a) c) => Coerce((Prod m n) a) (b,c) where
    down mnx = (down (pfst mnx), down(psnd mnx))
    up (x,y) = Prod(up x) (up y)

instance (Functor m, Functor n, Coerce(m b)c, Coerce(n a)b) => Coerce((Comp m n) a) c where
    down = down . fmap down . unComp
    up   = Comp . fmap up . up    

instance Coerce (m a) c => Coerce (WrappedMonad m a) c where
    down = down . unwrapMonad
    up   = WrapMonad . up

instance Coerce (State s a) (s -> (a,s)) where
    down = runState
    up = state

type Count = Const (Sum Integer)

count :: a -> Count b
count _ = Const 1

cciBody :: Char -> Count a
cciBody = count

cci :: String -> Count [a]
cci = traverse cciBody


lciBody :: Char -> Count a
lciBody c = up $ test (c == '\n')

test :: Bool -> Sum Integer
test b = Sum $ if b then 1 else 0

lci :: String -> Count [a]
lci = traverse lciBody

clci :: String -> Prod Count Count [a]
clci = traverse (cciBody <#> lciBody)

wciBody :: Char -> Comp (WrappedMonad (State Bool)) Count a
wciBody c = up (updateState c) where
    updateState :: Char -> Bool -> (Sum Integer, Bool)
    updateState c w = let s = not(isSpace c) in (test (not w && s), s)
    isSpace :: Char -> Bool
    isSpace c = c == ' ' || c == '\n' || c == '\t'

wci :: String -> Comp (WrappedMonad (State Bool)) Count [a]
wci = traverse wciBody

clwci :: String -> (Prod (Prod Count Count) (Comp (WrappedMonad (State Bool)) Count)) [a]
clwci = traverse (cciBody <#> lciBody <#> wciBody)

str :: String
str = "hello nice \n and busy world"

iteratorDemo = do
    putStrLn "Iterator -> Traversable"
    let exp = Mul (Add (Val 3) (Val 1)) 
                (Mul (Val 2) (Var "pi"))
        env = [("pi", pi)]
    print $ traverse (\x c -> if even x then [x] else [2*x]) exp 0
    print $ clci str
    let wordcount = clwci str
    print $ pfst wordcount
    print $ runState (unwrapMonad (unComp (psnd wordcount))) False
                            

    