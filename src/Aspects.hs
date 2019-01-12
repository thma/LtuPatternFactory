{-# LANGUAGE DeriveFunctor #-}
module Aspects where

--import           Prelude hiding (div)
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Writer

data Exp a =
      Var String
    | BinOp String (BinOperator a) (Exp a) (Exp a)
    | Let String (Exp a) (Exp a)
    | Val a

type BinOperator a =  a -> a -> a

type Env a = [(String, a)]

plus = BinOp "+" (+)
mul  = BinOp "*" (*)
divBy  = BinOp "/" (/)

-- environment lookup
fetch :: String -> Env a -> a
fetch x []        = error $ "variable " ++ x ++ " is not defined"
fetch x ((y,v):ys)
    | x == y    = v
    | otherwise = fetch x ys

eval :: Show a => Exp a -> WriterT [String] (Reader (Env a)) a           
eval (Var x)          = tell ["lookup " ++ x] >> asks (fetch x)
eval (Val i)          = tell [show i] >> return i
eval (BinOp n op e1 e2) = tell [n] >> liftM2 op (eval e1) (eval e2)
eval (Let x e1 e2)    = do 
    tell ["let " ++ x]
    v <- eval e1
    tell ["in"] 
    local ((x,v):) (eval e2)

aspectsDemo :: IO ()
aspectsDemo = do
    putStrLn "Interpreter -> Reader Monad + ADTs + pattern matching"
    let exp = Let "x" 
                (Let "y" 
                    (plus (Val 5) (Val 7))
                    (divBy  (Var "y") (Val 6)))
                (mul (Var "pi") (Var "x"))
        env = [("pi", pi)]
    print $ runReader (runWriterT (eval exp)) env

    --
    demo (run program)
    
    putStrLn ""

---------------------------
type Id = String 

data IExp = Lit Int
    | IExp :+: IExp
    | IExp :*: IExp
    | IExp :-: IExp
    | IExp :/: IExp
    | IVar Id deriving (Show)

data BExp = T
    | F
    | Not BExp
    | BExp :&: BExp
    | BExp :|: BExp
    | IExp :=: IExp
    | IExp :<: IExp deriving (Show)

data Stmt = Skip
    | Id := IExp
    | Begin [Stmt]
    | If BExp Stmt Stmt
    | While BExp Stmt deriving (Show)

program =
    Begin [
        "total" := Lit 0,
        "count" := Lit 0,
        While (IVar "count" :<: Lit 10)
            (Begin [
                "count" := (IVar "count" :+: Lit 1),
                "total" := (IVar "total" :+: IVar "count")
            ])
    ]    


type Store = Id -> Int

extend :: Store -> Id -> Int -> Store
extend s i v = \j -> if i == j then v else s j    

iexp :: IExp -> (Store -> Int)
iexp (Lit n) = const n
iexp (e1 :+: e2) = \s -> iexp e1 s + iexp e2 s
iexp (e1 :*: e2) = \s -> iexp e1 s * iexp e2 s
iexp (e1 :-: e2) = \s -> iexp e1 s - iexp e2 s
iexp (e1 :/: e2) = \s -> iexp e1 s `div` iexp e2 s
iexp (IVar i)     = \s -> s i

bexp :: BExp -> (Store -> Bool )
bexp T           = const True
bexp F           = const False
bexp (Not b)     = \s -> not (bexp b s)
bexp (b1 :&: b2) = \s -> bexp b1 s && bexp b2 s
bexp (b1 :|: b2) = \s -> bexp b1 s || bexp b2 s
bexp (e1 :=: e2) = \s -> iexp e1 s == iexp e2 s
bexp (e1 :<: e2) = \s -> iexp e1 s < iexp e2 s

stmt :: Stmt -> (Store -> Store)
stmt Skip       = \s -> s
stmt (i := e)   = \s -> extend s i (iexp e s)
stmt (Begin ss) = foldr (\s ss -> ss . stmt s) id ss
stmt (If b t e) = \s -> if bexp b s
                            then stmt t s
                            else stmt e s
stmt (While b t) = loop
    where loop s = if bexp b s
                    then loop (stmt t s)
                    else s

run :: Stmt -> Store
run s = stmt s (const 0)

data M r = ST (Store -> (r, Store))

{-
instance Applicative M where
    pure = return
    a <*> b = fmap f r
instance (Functor m, Monad m) => Applicative (StateT s m) where
    pure a = StateT $ \ s -> return (a, s)
    StateT mf <*> StateT mx = StateT $ \ s -> do
        ~(f, s') <- mf s
        ~(x, s'') <- mx s'
        return (f x, s'')    
instance Monad M where
    return x = ST (\s -> (x , s))
    c >>= g  = ST (\s -> let ST f     = c
                             (x , s') = f s
                             ST f'    = g x
                         in f' s')
-}

setVar :: Id -> Int -> M ()
setVar i v = ST (\s -> ((), \j -> if i == j then v else s j ))

getVar :: Id -> M Int
getVar i = ST (\s -> (s i, s))

-- demo (run program)
demo :: Store -> IO () 
demo store = do
    putStrLn $ "count = " ++ show (store "count")
    putStrLn $ "total = " ++ show (store "total")
    putStrLn $ "other = " ++ show (store "other")
