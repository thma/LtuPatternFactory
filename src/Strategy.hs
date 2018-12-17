{-#LANGUAGE DeriveFunctor #-}
module Strategy where

-- first we define two simple strategies that work on numbers:
strategyDouble :: Num a => a -> a
strategyDouble n = 2*n

strategySquare :: Num a => a -> a
strategySquare n = n*n

strategyToString :: Show a => a -> String
strategyToString = show

newtype Context a = Context a deriving (Functor, Show, Read)

applyInContext :: Num a => (a -> b) -> Context a -> Context b
applyInContext f (Context a) = Context (f a)

--instance Functor Context where
--    fmap f (Context a) = Context (f a)

-- | applyInListContext applies a function of type Num a => a -> a to a list of a's:
applyInListContext :: Num a => (a -> b) -> [a] -> [b]
-- applying f to an empty list returns the empty list
-- applyInListContext f [] = []
-- applying f to a list with head x returns (f x) 'consed' to a list
-- resulting from applying applyInListContext f to the tail of the list
-- applyInListContext f (x:xs) = (f x) : applyInListContext f xs

-- HLint, the Haskell linter advices us to use the predefined map function instead of this definition:
applyInListContext = map


strategyDemo = do
    putStrLn "Strategy Pattern -> Functor (and Higher Order Functions in general)"

    print $ strategySquare 16
    print $ (strategyToString . strategySquare . strategyDouble) 4

    print $ applyInContext (strategySquare . strategyDouble) (Context 7)

    print $ applyInListContext strategyDouble [1..10]
    print $ applyInListContext strategySquare [1..10]
    print $ applyInListContext (strategyToString . strategySquare . strategyDouble) [1..10]

    print $ fmap strategyDouble (Context 7)
    print $ fmap strategyDouble [7]
    putStrLn ""
