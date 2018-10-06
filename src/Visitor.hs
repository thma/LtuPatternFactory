{-# LANGUAGE DeriveFoldable #-}
module Visitor where
import Data.Monoid

data Exp a = 
    Val a
  | Add (Exp a) (Exp a)
  | Mul (Exp a) (Exp a)
    deriving (Show, Foldable)

{-
instance Foldable Exp where
    foldMap f (Val x)   = f x
    foldMap f (Add x y) = foldMap f x ++ foldMap f y
        where (++) = mappend
    foldMap f (Mul x y) = foldMap f x ++ foldMap f y
        where (++) = mappend
-}

size :: Foldable f => f a -> Int
size = getSum . foldMap (Sum . const 1)

exp = Mul (Add (Val 3) (Val 1)) 
          (Mul (Val 4) (Val pi))

visitorDemo = do
    putStrLn "Visitor vs. Foldable, Traversable"
    let exp = Mul (Add (Val 3) (Val 1)) 
                  (Mul (Val 4) (Val pi))
    print exp
    putStr "size of exp: "
    print (size exp)