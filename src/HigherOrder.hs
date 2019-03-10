module HigherOrder where

import Prelude hiding (sum, product, foldr, map, filter)    

type Lookup key value = key -> Maybe value

get :: Lookup k v
get _ = Nothing

put :: Eq k => k -> v -> Lookup k v -> Lookup k v
put k v lookup = 
    \key -> if key == k 
            then Just v 
            else lookup key

--
sum :: [Int] -> Int
sum []     = 0
sum (x:xs) = x + sum xs

product :: [Int] -> Int
product []     = 1
product (x:xs) = x * product xs

map :: (a -> b) -> [a] -> [b]
map _ []     = []
map f (x:xs) = f x : map f xs 

filter :: (a -> Bool) -> [a] -> [a]
filter _ [] = []
filter p (x:xs) = if p x then x : filter p xs else filter p xs

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f z []     = z
foldr f z (x:xs) = f x (foldr f z xs)

sum' :: [Int] -> Int
sum' = foldr (+) 0

product' :: [Int] -> Int
product' = foldr (*) 1

map' :: (a -> b) -> [a] -> [b]
map' f = foldr ((:) . f) []

filter' :: (a -> Bool) -> [a] -> [a]
filter' p = foldr (\x xs -> if p x then x : xs else xs) []

higherOrderDemo :: IO ()
higherOrderDemo = do
    putStrLn "higher order functions"
    let get   = put "a" 1 (const Nothing)
        get'  = put "b" 2 get
        get'' = put "c" 3 get'
    print $ get'' "a"
    print $ get'' "b"
    print $ get'' "c"
    print $ get'' "d"

    print $ sum' [1..10]
    print $ product' [1..10]
    print $ map' (*2) [1..10]
    print $ filter' even [1..10]


