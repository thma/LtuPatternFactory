module Strategy where

-- first we define two simple strategies that map numbers to numbers:
strategyId :: Num a => a -> a
strategyId n = n

strategyDouble :: Num a => a -> a
strategyDouble n = 2*n

-- now we define a context that applies a function of type Num a => a -> a to a list of a's:
context :: Num a => (a -> a) -> [a] -> [a]
context = fmap

strategyDemo = do
    putStrLn "Strategy Pattern -> Functor (and Higher Order Functions in general)"
    print $ context strategyId [1..10]
    print $ context strategyDouble [1..10]
    putStrLn ""