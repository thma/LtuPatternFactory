-- The DeriveFunctor Language Pragma provides automatic derivation of Functor instances
{-# LANGUAGE DeriveFunctor #-}
module Pipeline where

-- The Stream type is a wrapper around an arbitrary payload type 'a'
newtype Stream a = Stream a deriving (Show)

-- echo injects an item of type 'a' into the Stream context
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

-- a log is just a list of Strings
type Log = [String]

-- the Stream type is extended by a Log, that keeps track of any logged messages
newtype LoggerStream a = LoggerStream (a, Log) deriving (Show, Functor)

instance Applicative LoggerStream where
  pure = return
  LoggerStream (f, _) <*> r = fmap f r

-- our definition of the Logging Stream Monad
instance Monad LoggerStream where
  -- returns a Stream wrapping a tuple of the actual payload and an empty Log
  return a = LoggerStream (a, [])
  -- we define (>>=) to return a tuple (composed functions, concatenated logs)
  m1 >>= m2  = let LoggerStream(f1, l1) = m1
                   LoggerStream(f2, l2) = m2 f1
               in  LoggerStream(f2, l1 ++ l2)

-- compute length of a String and provide a log message
logLength :: String -> LoggerStream Int
logLength str = let l = length(words str)
                in LoggerStream (l, ["length(" ++ str ++ ") = " ++ show l])

logMultiply :: Int -> LoggerStream Int
logMultiply x = let z = x * 3
                in LoggerStream (z, ["multiply(" ++ show x ++ ", 3" ++") = " ++ show z])

logPipeline :: String -> LoggerStream Int
logPipeline str =
  return str >>= logLength >>= logMultiply

pipelineDemo = do
    putStrLn "Pipeline -> Monad"
    print $ pipeline "hello world"
    print $ logPipeline "hello logging world"
    putStrLn ""
