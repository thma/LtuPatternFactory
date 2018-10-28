module Iterator where
import Singleton (Exp (..))
import Visitor
import Data.Functor.Const
import Data.Monoid (Sum (..), getSum)

instance Functor Exp where
    fmap f (Var x)   = Var x
    fmap f (Val a)   = Val $ f a
    fmap f (Add x y) = Add (fmap f x) (fmap f y)
    fmap f (Mul x y) = Mul (fmap f x) (fmap f y)

instance Traversable Exp where
    traverse g (Var x)   = pure $ Var x
    traverse g (Val x)   = Val <$> g x
    traverse g (Add l r) = Add <$> traverse g l
                               <*> traverse g r
    traverse g (Mul l r) = Mul <$> traverse g l
                               <*> traverse g r

str :: [Char]
str = "hello \n nice \t and \n busy world"



type Count = Const (Sum Integer)
count :: a -> Count b
count _ = Const 1

cciBody :: Char -> Count a
cciBody = count

cci :: String -> Count [a]
cci = traverse cciBody

lciBody :: Char -> Count a
lciBody c = Const (if c == '\n' then 1 else 0)

lci :: String -> Count [a]
lci = traverse lciBody

data Prod m n a = Prod {pfst:: m a, psnd:: n a} deriving (Show)

x :: (Functor m, Functor n) => (a -> m b) -> (a -> n b) -> (a -> Prod m n b)
(f `x` g) y = Prod (f y) (g y) 

instance (Functor m, Functor n) => Functor (Prod m n) where    
  fmap f (Prod m n) = Prod (fmap f m) (fmap f n)

instance (Applicative m, Applicative n) => Applicative (Prod m n) where
    pure x = Prod (pure x) (pure x)
    mf <*> mx = Prod (pfst mf <*> pfst mx) (psnd mf <*> psnd mx)

--wciBody :: Char -> (Monad (State Bool) . Count) a

clci :: String -> Prod Count Count [a]
clci = traverse (cciBody `x` lciBody)

iteratorDemo = do
    putStrLn "Iterator -> Traversable"
    let exp = Mul (Add (Val 3) (Val 1)) 
                  (Mul (Val 2) (Var "pi"))
        env = [("pi", pi)]
    print $ traverse (\x -> if even x then [x] else [2*x]) exp
    --print $ traverse count str
                            