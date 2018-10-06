module Visitor where
import Singleton (Exp (..))
import Data.Monoid (Sum (..), getSum)

-- we are re-using the Exp data type from the Singleton example 
-- and transform it into a Foldable type:
instance Foldable Exp where
    foldMap f (Val x)   = f x
    foldMap f (Add x y) = foldMap f x `mappend` foldMap f y
    foldMap f (Mul x y) = foldMap f x `mappend` foldMap f y

-- instead of size we could just use the predefined Foldable.length
size :: Foldable f => f a -> Int
size = getSum . foldMap (Sum . const 1)

visitorDemo = do
    putStrLn "Visitor vs. Foldable, Traversable"
    let exp = Mul (Add (Val 3) (Val 1)) 
                  (Mul (Val 4) (Val pi))
    print exp
    putStr "size of exp: "
    print (size exp)