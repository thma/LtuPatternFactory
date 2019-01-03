module Combinators where

k :: a -> b -> a
k x _ = x

s :: (a -> b -> c) -> (a -> b) -> a -> c
s x y z = x z (y z)

i :: a -> a
i = s k k

combinatorDemo :: IO ()
combinatorDemo = do
  putStrLn "SKI combinators\n"
  print $ s k k 3
  putStrLn ""
