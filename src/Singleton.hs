module Singleton 
    (
        singletonDemo
    )
where

data Exp = Var String
    | Val Double
    | Add Exp Exp
    | Mul Exp Exp

type Env = [(String, Double)]

-- the naive implementation of eval:
-- the environment is threaded into each recursive call of eval
-- as an explicit parameter e
eval :: Exp -> Env -> Double
eval (Var x)   e = fetch x e
eval (Val i)   e = i
eval (Add p q) e = eval p e + eval q e
eval (Mul p q) e = eval p e * eval q e

-- the K combinator
k :: a -> env -> a
k x e = x
-- the S combinator
s :: (env -> a -> b) -> (env -> a) -> env -> b
s ef es e = ef e (es e)

-- the SK combinator based implementation
-- the threading of the env into recursive calls is by the S combinator
-- currying allows to omit the explicit parameter e
eval1 :: Exp -> Env -> Double
eval1 (Var x)   = fetch x
eval1 (Val i)   = k i
eval1 (Add p q) = k (+) `s` eval1 p `s` eval1 q
eval1 (Mul p q) = k (*) `s` eval1 p `s` eval1 q

-- instance Applicative ((->) r) where
-- pure x _ = x
-- f <*> g  = \x -> f x (g x)

-- applicative functor based implementation
-- the K and S magic is now done by pure and <*>
eval2 :: Exp -> Env -> Double
eval2 (Var x)   = fetch x
eval2 (Val i)   = pure i
eval2 (Add p q) = pure (+) <*> eval2 p  <*> eval2 q
eval2 (Mul p q) = pure (*) <*> eval2 p  <*> eval2 q

-- simple environment lookup
fetch :: String -> Env -> Double
fetch x []        = error $ "variable " ++ x ++ " is not defined"
fetch x ((y,v):ys)
    | x == y    = v
    | otherwise = fetch x ys

singletonDemo :: IO ()
singletonDemo = do
    putStrLn "Singleton vs. Applicative Functor, Pointed (and let in general)"
    let exp = Mul (Add (Val 3) (Val 1)) 
                  (Mul (Val 2) (Var "pi"))
        env = [("pi", pi)]
    print $ eval exp env
    putStrLn ""