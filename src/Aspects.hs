{-# LANGUAGE DeriveFunctor #-}
module Aspects where

type Id = String 

data IExp = Lit Int
    | IExp :+: IExp
    | IExp :*: IExp
    | IExp :-: IExp
    | IExp :/: IExp
    | Var Id deriving (Show)

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
        While (Var "count" :<: Lit 10)
            (Begin [
                "count" := (Var "count" :+: Lit 1),
                "total" := (Var "total" :+: Var "count")
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
iexp (Var i)     = \s -> s i

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