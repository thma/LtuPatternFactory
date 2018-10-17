module Visitor where
import Singleton (Exp (..))
import Data.Monoid (Sum (..), getSum)

-- we are re-using the Exp data type from the Singleton example 
-- and transform it into a Foldable type:
instance Foldable Exp where
    foldMap f (Val x)   = f x
    foldMap f (Add x y) = foldMap f x `mappend` foldMap f y
    foldMap f (Mul x y) = foldMap f x `mappend` foldMap f y

filterF :: Foldable f => (a -> Bool) -> f a -> [a]
filterF p = foldMap (\a -> if p a then [a] else [])     

visitorDemo = do
    putStrLn "Visitor -> Foldable -> Traversable"
    let exp = Mul (Add (Val 3) (Val 2)) 
                  (Mul (Val 4) (Val 6))
    print exp
    putStr "size of exp: "
    print $ length exp
    putStrLn "filter even numbers from tree"
    print $ filterF even exp


