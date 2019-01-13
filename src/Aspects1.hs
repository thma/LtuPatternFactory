{-# LANGUAGE FlexibleContexts #-}
module Aspects1 where
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Writer
import           Data.Map    (Map, fromList)
import qualified Data.Map    as Map (lookup, insert)
import           Aspects (Id, IExp (..), BExp (..), Stmt (..), Store (..), program, lookup', demo, getVar, setVar)

data JoinPointDesc = Get Id | Set Id

data PointCut = Setter
              | Getter
              | AtVar Id
              | NotAt PointCut
              | PointCut :||: PointCut
              | PointCut :&&: PointCut

includes :: PointCut -> (JoinPointDesc -> Bool)
includes Setter (Set i)    = True
includes Getter (Get i)    = True
includes (AtVar i) (Get j) = i == j
includes (AtVar i) (Set j) = i == j
includes (NotAt p) d       = not (includes p d)
includes (p :||: q) d      = includes p d || includes q d
includes (p :&&: q) d      = includes p d && includes q d
includes _ _               = False

data Advice = Before PointCut Stmt
            | After  PointCut Stmt

countSets = After ((Setter :&&: (NotAt (AtVar "setters"))) :&&: (NotAt (AtVar "getters")))
                  ("setters" := (IVar "setters" :+: Lit 1))

countGets = After ((Getter :&&: (NotAt (AtVar "setters"))) :&&: (NotAt (AtVar "getters")))
                  ("getters" := (IVar "getters" :+: Lit 1))

type Aspects = [Advice]


iexp :: IExp -> ReaderT Aspects (State Store) Int
iexp (Lit n) = return n
iexp (e1 :+: e2) = liftM2 (+) (iexp e1) (iexp e2)
iexp (e1 :*: e2) = liftM2 (*) (iexp e1) (iexp e2)
iexp (e1 :-: e2) = liftM2 (-) (iexp e1) (iexp e2)
iexp (e1 :/: e2) = liftM2 div (iexp e1) (iexp e2)
iexp (IVar i)    = withAdvice (Get i) (getVar i)

bexp :: BExp -> ReaderT Aspects (State Store) Bool
bexp T           = return True
bexp F           = return False
bexp (Not b)     = fmap not (bexp b)
bexp (b1 :&: b2) = liftM2 (&&) (bexp b1) (bexp b2)
bexp (b1 :|: b2) = liftM2 (||) (bexp b1) (bexp b2)
bexp (e1 :=: e2) = liftM2 (==) (iexp e1) (iexp e2)
bexp (e1 :<: e2) = liftM2 (<)  (iexp e1) (iexp e2)

stmt :: Stmt -> ReaderT Aspects (State Store) ()
stmt Skip       = return ()
stmt (i := e)   = do x <- iexp e; withAdvice (Set i) (setVar i x)
stmt (Begin ss) = mapM_ stmt ss
stmt (If b t e) = do 
    x <- bexp b
    if x then stmt t
         else stmt e
stmt (While b t) = loop
    where loop = do 
            x <- bexp b
            when x $ stmt t >> loop

withAdvice d c = do
    aspects <- ask
    mapM_ stmt (before d aspects)
    x <- c
    mapM_ stmt (after d aspects)
    return x

before, after :: JoinPointDesc -> Aspects -> [Stmt]
before d as = [s | Before c s <- as, includes c d] 
after  d as = [s | After  c s <- as, includes c d] 


run :: Aspects -> Stmt -> Store
run a s = execState (runReaderT (stmt s) a) (fromList [])

aspects1Demo :: IO ()
aspects1Demo = do
    putStrLn "Aspect Weaving -> Monad Transformers"
    demo (run [countSets] program)
    demo (run [countGets] program)
    demo (run [countSets, countGets] program)
    putStrLn ""