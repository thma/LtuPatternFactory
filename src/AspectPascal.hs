{-# LANGUAGE FlexibleContexts #-}
module AspectPascal where
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Writer
import           Data.Map   (Map)
import qualified Data.Map   as Map (lookup, insert, fromList)
import           MiniPascal (Id, IExp (..), BExp (..), Stmt (..), Store (..)
                            , program, demo, getVar, setVar)

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

-- the countSets Advice traces each setting of a variable and increments the counter "countSet"            
countSets = After (Setter :&&: NotAt (AtVar "countSet") :&&: NotAt (AtVar "countGet"))
                  ("countSet" := (IVar "countSet" :+: Lit 1))

-- the countGets Advice traces each lookup of a variable and increments the counter "countGet"            
countGets = After (Getter :&&: NotAt (AtVar "countSet") :&&: NotAt (AtVar "countGet"))
                  ("countGet" := (IVar "countGet" :+: Lit 1))

type Aspects = [Advice]

iexp :: MonadState Store m => IExp -> ReaderT Aspects m Int
iexp (Lit n) = return n
iexp (e1 :+: e2) = liftM2 (+) (iexp e1) (iexp e2)
iexp (e1 :*: e2) = liftM2 (*) (iexp e1) (iexp e2)
iexp (e1 :-: e2) = liftM2 (-) (iexp e1) (iexp e2)
iexp (e1 :/: e2) = liftM2 div (iexp e1) (iexp e2)
iexp (IVar i)    = withAdvice (Get i) (getVar i)

bexp :: MonadState Store m => BExp -> ReaderT Aspects m Bool
bexp T           = return True
bexp F           = return False
bexp (Not b)     = fmap not (bexp b)
bexp (b1 :&: b2) = liftM2 (&&) (bexp b1) (bexp b2)
bexp (b1 :|: b2) = liftM2 (||) (bexp b1) (bexp b2)
bexp (e1 :=: e2) = liftM2 (==) (iexp e1) (iexp e2)
bexp (e1 :<: e2) = liftM2 (<)  (iexp e1) (iexp e2)

stmt :: MonadState Store m => Stmt -> ReaderT Aspects m ()
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

withAdvice :: MonadState Store m => JoinPointDesc -> ReaderT Aspects m a -> ReaderT Aspects m a
withAdvice joinPoint action = do
    aspects <- ask
    mapM_ stmt (before joinPoint aspects)
    x <- action
    mapM_ stmt (after joinPoint aspects)
    return x

before, after :: JoinPointDesc -> Aspects -> [Stmt]
before joinPoint aspects = [s | Before pointCut s <- aspects, includes pointCut joinPoint] 
after  joinPoint aspects = [s | After  pointCut s <- aspects, includes pointCut joinPoint] 

run :: Aspects -> Stmt -> Store
run a s = execState (runReaderT (stmt s) a) (Map.fromList [])

aspectPascalDemo :: IO ()
aspectPascalDemo = do
    putStrLn "Aspect Weaving -> Monad Transformers"
    demo (run [countSets] program)
    demo (run [countGets] program)
    demo (run [countSets, countGets] program)
    putStrLn ""