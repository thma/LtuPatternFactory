-- The DeriveFunctor Language Pragma provides automatic derivation of Functor instances
{-# LANGUAGE DeriveFunctor #-}
module Pipeline where

-- The Stream type is a wrapper around an arbitrary payload type 'a'
newtype Stream a = Stream a deriving (Show)

-- echo lifts an item of type 'a' into the Stream context
echo :: a -> Stream a
echo = Stream

-- the 'andThen' operator used for chaining commands
infixl 7 |>
(|>) :: Stream a -> (a -> Stream b) -> Stream b
Stream x |> f = f x


-- echo and |> are used to create the actual pipeline
pipeline :: String -> Stream Int
pipeline str = 
  echo str |> echo . length . words |> echo . (3 *)


-- the Stream type is extened by an Int that keeps the counter state
newtype CountingStream a = CountingStream (a, Int) deriving (Show, Functor)

-- as any Monad must be an Applicative we also have to instantiate Applicative
instance Applicative CountingStream where
  pure = return
  CountingStream (f, _) <*> r = fmap f r

-- our definition of the Stream Monad
instance Monad CountingStream where
  -- returns a Stream wrapping a tuple of the actual payload and an initial counter state of 0
  return a = CountingStream (a, 0)
  -- we define (>>=) to reach an incremented counter to the subsequent action
  m >>= k = let CountingStream(a, c1) = m
                next                  = k a
                CountingStream(b, c2) = next
            in CountingStream (b, c1 + 1 + c2)

-- instead of echo and |> we now use the "official" monadic versions return and >>=
countingPipeline :: String -> CountingStream Int
countingPipeline str =
  return str >>= return . length . words >>= return . (3 *)

pipelineDemo = do 
    putStrLn "Pipeline vs. Monad"
    print $ pipeline "hello world"
    print $ countingPipeline "hello counting world"
    putStrLn ""