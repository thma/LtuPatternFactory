module Iterator where
import Singleton (Exp (..))
import Visitor

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

iteratorDemo = do
    putStrLn "Iterator -> Traversable"
    let exp = Mul (Add (Val 3) (Val 1)) 
                  (Mul (Val 2) (Var "pi"))
        env = [("pi", pi)]
    print $ traverse (\x -> if even x then [x] else [2*x]) exp
                            