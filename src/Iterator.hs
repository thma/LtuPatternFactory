module Iterator where
import           Singleton                (Exp (..))
import           Visitor

import           Control.Applicative
import           Control.Monad.State.Lazy
import           Data.Coerce              (coerce)
import           Data.Functor.Compose
import           Data.Functor.Const
import           Data.Functor.Identity
import           Data.Functor.Product
import           Data.Monoid              (Sum (..), getSum)

instance Functor Exp where
    fmap f (Var x)   = Var x
    fmap f (Val a)   = Val $ f a
    fmap f (Add x y) = Add (fmap f x) (fmap f y)
    fmap f (Mul x y) = Mul (fmap f x) (fmap f y)

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

type Count = Const (Sum Integer)

count :: a -> Count b
count _ = Const 1

cciBody :: Char -> Count a
cciBody = count

cci :: String -> Count [a]
cci = traverse cciBody

test :: Bool -> Sum Integer
test b = Sum $ if b then 1 else 0

lciBody :: Char -> Count a
lciBody c = Const $ test (c == '\n')

lci :: String -> Count [a]
lci = traverse lciBody

clci :: String -> Product Count Count [a]
clci = traverse (cciBody <#> lciBody)

wciBody :: Char -> Compose (WrappedMonad (State Bool)) Count a
wciBody c =  coerce (updateState c) where
    updateState :: Char -> Bool -> (Sum Integer, Bool)
    updateState c w = let s = not(isSpace c) in (test (not w && s), s)
    isSpace :: Char -> Bool
    isSpace c = c == ' ' || c == '\n' || c == '\t'

wci :: String -> Compose (WrappedMonad (State Bool)) Count [a]
wci = traverse wciBody

clwci :: String -> (Product (Product Count Count) (Compose (WrappedMonad (State Bool)) Count)) [a]
clwci = traverse (cciBody <#> lciBody <#> wciBody)

-- | the actual wordcount implementation.
--   for any String a triple of linecount, wordcount, charactercount is returned
wc :: String -> (Integer, Integer, Integer)
wc str =
    let raw = clwci str
        cc  = coerce $ pfst (pfst raw)
        lc  = coerce $ psnd (pfst raw)
        wc  = coerce $ evalState (unwrapMonad (getCompose (psnd raw))) False
    in (lc,wc,cc)

str :: String
str = "hello \n world"

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

    print $ wc str


