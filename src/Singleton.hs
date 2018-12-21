{-# LANGUAGE FlexibleContexts #-}
module Singleton where
import           IdiomBrackets
import           Control.Monad.Reader
import           Control.Monad.State

data Exp a =
      Var String
    | Def String a
    | Val a
    | Add (Exp a) (Exp a)
    | Mul (Exp a) (Exp a) deriving (Show)

type Env a = [(String, a)]

-- the naive implementation of eval:
-- the environment is threaded into each recursive call of eval
-- as an explicit parameter e
eval :: (Num a) => Exp a -> Env a -> a
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
-- the threading of the env into recursive calls is done by the S combinator
-- currying allows to omit the explicit parameter e
eval1 :: (Num a) => Exp a -> Env a -> a
eval1 (Var x)   = fetch x
eval1 (Val i)   = k i
eval1 (Add p q) = k (+) `s` eval1 p `s` eval1 q
eval1 (Mul p q) = k (*) `s` eval1 p `s` eval1 q

-- instance Applicative ((->) r) where
-- pure x _ = x
-- f <*> g  = \x -> f x (g x)

-- applicative functor based implementation
-- the K and S magic is now done by pure and <*>
eval2 :: (Num a) => Exp a -> Env a -> a
eval2 (Var x)   = fetch x
eval2 (Val i)   = pure i
eval2 (Add p q) = pure (+) <*> eval2 p  <*> eval2 q
eval2 (Mul p q) = pure (*) <*> eval2 p  <*> eval2 q

-- using the Idiom Bracket syntax suggested by Conor McBride
-- 'iI f a b ... Ii' stands for '[[f a b ...]]' which denotes 'pure f <*> a <*> b <*> ...'
eval3 :: (Num a) => Exp a -> Env a -> a
eval3 (Var x)   = fetch x
eval3 (Val i)   = iI i Ii
eval3 (Add p q) = iI (+) (eval3 p) (eval3 q) Ii
eval3 (Mul p q) = iI (*) (eval3 p) (eval3 q) Ii

-- using a Reader Monad to thread the environment. The Environment can be accessed by ask.
eval4 :: Num a => Exp a -> Env a -> a
eval4 (Var x)   = ask >>= return $ fetch x
eval4 (Val i)   = return i
eval4 (Add p q) = liftM2 (+) (eval4 p) (eval4 q)
eval4 (Mul p q) = liftM2 (*) (eval4 p) (eval4 q)

--eval5 :: Num a => Exp a -> Env a -> a
eval5 :: (MonadState (Env a) m, Num a) => Exp a -> m a
eval5 (Var x)   = do 
    env <- get 
    return $ fetch x env
eval5 (Def k v) = do
    env <- get
    put ((k,v):env)
    return v
eval5 (Val i)   = return i
eval5 (Add p q) = liftM2 (+) (eval5 p) (eval5 q)
eval5 (Mul p q) = liftM2 (*) (eval5 p) (eval5 q)

-- simple environment lookup
fetch :: String -> Env a -> a
fetch x []        = error $ "variable " ++ x ++ " is not defined"
fetch x ((y,v):ys)
    | x == y    = v
    | otherwise = fetch x ys

exp1 = Mul (Add (Def "pi" pi) (Val 1))
    (Mul (Val 2) (Var "pi"))

singletonDemo :: IO ()
singletonDemo = do
    putStrLn "Singleton -> Applicative Functor (and let in general)"
    let exp = Mul (Add (Val 3) (Val 1))
                  (Mul (Val 2) (Var "pi"))
        env = [("pi", pi)]
    print $ eval exp env
    print $ eval1 exp env
    print $ eval2 exp env
    print $ eval3 exp env

    print $ eval4 exp env

    let exp1 = Mul (Add (Def "pi" 4) (Val 1))
                  (Mul (Val 2) (Var "pi"))
   -- print $ eval5 exp (state env)

    putStrLn ""
