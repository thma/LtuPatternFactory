module Infinity where

pythagoreanTriples :: [(Int,Int,Int)] 
pythagoreanTriples = [ (a,b,c) | 
    c <- [1..], 
    b <- [1..c-1], 
    a <- [1..b-1], 
    a^2 + b^2 == c^2]  
  

infinityDemo :: IO ()
infinityDemo = do
    putStrLn "Infinite structures and computations -> laziness & list comprehension\n"
    print $ take 100 pythagoreanTriples
    putStrLn ""
