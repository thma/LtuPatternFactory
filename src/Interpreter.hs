{-# LANGUAGE FlexibleContexts #-}
module Interpreter where
import           Control.Monad.Reader
import           Control.Monad.State

data Exp a =
      Var String
    | Op (Operator a) (Exp a) (Exp a)
    | Let String (Exp a) (Exp a)
    | Val a

type Operator a =  a -> a -> a

type Env a = [(String, a)]

-- using a Reader Monad to thread the environment. The Environment can be accessed by ask.
--eval :: Real a => Exp a -> Env a -> a
eval (Var x)       = asks (fetch x)
eval (Val i)       = return i
eval (Op op e1 e2) = liftM2 op (eval e1) (eval e2)
eval (Let x e1 e2) = asks (eval e1) >>= \v -> local ((x,v):) (eval e2)

    
{--
    env <- ask
    v   <- eval e1
    local ((x,v):) (eval e2)
--}

{--
--eval5 :: Num a => Exp a -> Env a -> a
eval5 :: (MonadState (Env a) m, Num a) => Exp a -> m a
eval5 (Var x)   = 
    gets (fetch x)
eval5 (Def k v) = do
    env <- get
    put ((k,v):env)
    return v
eval5 (Val i)   = return i
eval5 (Add p q) = liftM2 (+) (eval5 p) (eval5 q)
eval5 (Mul p q) = liftM2 (*) (eval5 p) (eval5 q)
--}

-- simple environment lookup
fetch :: String -> Env a -> a
fetch x []        = error $ "variable " ++ x ++ " is not defined"
fetch x ((y,v):ys)
    | x == y    = v
    | otherwise = fetch x ys

interpreterDemo :: IO ()
interpreterDemo = do
    putStrLn "Interpreter -> Reader Monad + ADTs + pattern matching"
    let exp = Let "x" 
                (Let "y" 
                    (Op (+) (Val 5) (Val 6))
                    (Op (/) (Var "y") (Val 5)))
                (Op (*) (Val 3) (Var "x"))
        env = [("pi", pi)]
    print $ eval exp env
    --print $ runReader (eval exp) env
    --print $ eval5 exp env


    --print $ eval5 exp (put env :: State (Env Double) ())

    putStrLn ""
