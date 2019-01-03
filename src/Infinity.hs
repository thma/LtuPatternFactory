module Infinity where

-- | a list of all integer pythagorean triples with a² + b² = c² 
pythagoreanTriples :: [(Int, Int, Int)]
pythagoreanTriples = [ (a, b, c)
  | c <- [1 ..]
  , b <- [1 .. c-1]
  , a <- [1 .. b-1]
  , a^2 + b^2 == c^2
  ]

-- | bottom, a computation which never completes successfully, aka as _|_
bottom :: a
bottom = bottom

-- | a CAF representing all integer numbers (https://wiki.haskell.org/Constant_applicative_form)
ints :: Num a => [a]
ints = from 1
  where
    from n = n : from (n + 1)

-- | the K combinator which drop its second argument
k :: a -> b -> a
k x _ = x

infinityDemo :: IO ()
infinityDemo = do
  putStrLn
    "Infinite data structures and nonterminating computations -> laziness & list comprehension\n"
  print $ take 100 ints
  print $ take 10 [2,4 ..]
  print $ take 10 pythagoreanTriples
  print $ k "21 is just half the truth" undefined
  print $ k 42 bottom
  putStrLn ""
