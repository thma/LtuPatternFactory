{-# LANGUAGE FlexibleContexts #-}
module Aspects where
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Writer
import           Data.Map    (Map, fromList, assocs)
import qualified Data.Map    as Map (lookup, insert)
import           Interpreter hiding (eval)

eval :: Show a => Exp a -> WriterT [String] (Reader (Env a)) a           
eval (Var x)          = tell ["lookup " ++ x] >> asks (fetch x)
eval (Val i)          = tell [show i] >> return i
eval (BinOp op e1 e2) = tell ["Op"] >> liftM2 op (eval e1) (eval e2)
eval (Let x e1 e2)    = do 
    tell ["let " ++ x]
    v <- eval e1
    tell ["in"] 
    local ((x,v):) (eval e2)

-- Mini Pascal --
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

type Store = Map Id Int

lookup' :: Ord a => Map a b -> a -> Maybe b
lookup' = flip Map.lookup

iexp :: IExp -> State Store Int
iexp (Lit n) = return n
iexp (e1 :+: e2) = liftM2 (+) (iexp e1) (iexp e2)
iexp (e1 :*: e2) = liftM2 (*) (iexp e1) (iexp e2)
iexp (e1 :-: e2) = liftM2 (-) (iexp e1) (iexp e2)
iexp (e1 :/: e2) = liftM2 div (iexp e1) (iexp e2)
iexp (IVar i)    = getVar i

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
stmt (i := e)   = do x <- iexp e; setVar i x
stmt (Begin ss) = mapM_ stmt ss
stmt (If b t e) = do 
    x <- bexp b
    if x then stmt t
         else stmt e
stmt (While b t) = loop
    where loop = do 
            x <- bexp b
            when x $ stmt t >> loop

setVar :: (MonadState (Map k a) m, Ord k) => k -> a -> m ()
setVar i x = do
    store <- get 
    put (Map.insert i x store)       

getVar :: MonadState Store m => Id -> m Int
getVar i = do
    s <- get
    case lookup' s i of
        Nothing  -> return 0
        (Just v) -> return v
        

run :: Stmt -> Store
run s = execState (stmt s) (fromList [])

demo :: Store -> IO () 
demo store = print (assocs store)

aspectsDemo :: IO ()
aspectsDemo = do
    putStrLn "Aspect Weaving -> Monad Transformers"
    let env = [("pi", pi)]
    print $ runReader (runWriterT (eval letExp)) env

    demo (run program)
    
    putStrLn ""