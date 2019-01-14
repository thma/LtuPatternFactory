{-# LANGUAGE FlexibleContexts #-}
module Interpreter where
import           Control.Monad.Reader
import           Control.Monad.State

data Exp a =
      Var String
    | BinOp (BinOperator a) (Exp a) (Exp a)
    | Let String (Exp a) (Exp a)
    | Val a

type BinOperator a =  a -> a -> a

type Env a = [(String, a)]

-- environment lookup
fetch :: String -> Env a -> a
fetch x []        = error $ "variable " ++ x ++ " is not defined"
fetch x ((y,v):ys)
    | x == y    = v
    | otherwise = fetch x ys

-- using a Reader Monad to thread the environment. The Environment can be accessed by ask and asks.
--eval :: Exp a -> Env a -> a
--eval :: Exp a -> ((->) (Env a)) a
eval :: MonadReader (Env a) m => Exp a -> m a                 
eval (Var x)          = asks (fetch x)
eval (Val i)          = return i
eval (BinOp op e1 e2) = liftM2 op (eval e1) (eval e2)
eval (Let x e1 e2)    = eval e1 >>= \v -> local ((x,v):) (eval e2)

eval' :: Exp a -> Reader (Env a) a               
eval' (Var x)          = asks (fetch x)
eval' (Val i)          = return i
eval' (BinOp op e1 e2) = liftM2 op (eval' e1) (eval' e2)
eval' (Let x e1 e2)    = eval' e1 >>= \v -> local ((x,v):) (eval' e2)

-- using a State Monad to thread the environment. The Environment can be accessed by get, gets, modify.
eval1 :: (MonadState (Env a) m) => Exp a -> m a
eval1 (Val i)          = return i
eval1 (Var x)          = gets (fetch x)
eval1 (BinOp op e1 e2) = liftM2 op (eval1 e1) (eval1 e2)
eval1 (Let x e1 e2)    = eval1 e1 >>= \v -> modify ((x,v):) >> eval1 e2

letExp = Let "x" 
            (Let "y" 
                (BinOp (+) (Val 5) (Val 7))
                (BinOp (/) (Var "y") (Val 6)))
            (BinOp (*) (Var "pi") (Var "x"))

interpreterDemo :: IO ()
interpreterDemo = do
    putStrLn "Interpreter -> Reader Monad + ADTs + pattern matching"
    let env = [("pi", pi)]
    print $ eval letExp env
    print $ runReader (eval letExp) env

    print $ runState (eval1 letExp) env

    let exp1 = Let "x" (BinOp (+) (Val 4) (Val 5)) (BinOp (*) (Val 2) (Var "x"))
    print $ eval exp1 []

    putStrLn ""
