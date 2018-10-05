{-# LANGUAGE DeriveFoldable #-}
module Visitor where
--import Data.Foldable

data Exp a = 
    Val a
    | Add (Exp a) (Exp a)
    | Mul (Exp a) (Exp a)
    deriving (Show, Foldable)
{-
instance Foldable Exp where
    foldMap f Empty = mempty
    foldMap f (Val x) = f x
    foldMap f (Add x y) = foldMap f x ++ foldMap f y
        where (++) = mappend
    foldMap f (Mul x y) = foldMap f x ++ foldMap f y
        where (++) = mappend
-}

visitorDemo = do
    putStrLn "Visitor vs. Foldable, Traversable"
    let exp = Mul (Add (Val 3) (Val 1)) 
                  (Mul (Val 4) (Val pi))
    print exp
    putStr "size of exp: "
    print (length exp)