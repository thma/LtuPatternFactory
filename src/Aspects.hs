module Aspects where
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
    putStrLn "Aspect Weaving -> Monad Transformers"
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


type Store = [(Id,Int)]

extend :: Store -> Id -> Int -> Store
extend s i v = (i,v):s

find :: Store -> Id -> Int
find ((k,v):xs) i = if k == i then v else find xs i

iexp :: IExp -> State Store Int
iexp (Lit n) = return n
iexp (e1 :+: e2) = liftM2 (+) (iexp e1) (iexp e2)
iexp (e1 :*: e2) = liftM2 (*) (iexp e1) (iexp e2)
iexp (e1 :-: e2) = liftM2 (-) (iexp e1) (iexp e2)
iexp (e1 :/: e2) = liftM2 div (iexp e1) (iexp e2)
iexp (IVar i)    = do
                        s <- get
                        return $ find s i

bexp :: BExp -> State Store Bool
bexp T           = return True
bexp F           = return False
bexp (Not b)     = fmap not (bexp b)
bexp (b1 :&: b2) = liftM2 (&&) (bexp b1) (bexp b2)
bexp (b1 :|: b2) = liftM2 (||) (bexp b1) (bexp b2)
bexp (e1 :=: e2) = liftM2 (==) (iexp e1) (iexp e2)
bexp (e1 :<: e2) = liftM2 (<)  (iexp e1) (iexp e2)

stmt :: Stmt -> State Store ()
stmt Skip       = return ()
stmt (i := e)   = do x <- iexp e; s <- get; put (extend s i x)
stmt (Begin ss) = mapM_ stmt ss
stmt (If b t e) = do 
    x <- bexp b
    if x then stmt t
         else stmt e
stmt (While b t) = loop
    where loop = do 
            x <- bexp b
            when x $ stmt t >> loop

run :: Stmt -> Store
run s = execState (stmt s) []


-- demo (run program)
demo :: Store -> IO () 
demo store = do
    putStrLn $ "count = " ++ show (find store "count")
    putStrLn $ "total = " ++ show (find store "total")
--    putStrLn $ "other = " ++ show (find store "other")
