{-# LANGUAGE DeriveFoldable #-}
module HigherOrder where

import Prelude hiding (sum, product, map, filter, foldr)
import Data.List (unfoldr)

type Lookup key value = key -> Maybe value

nada :: Lookup k v
nada _ = Nothing

abc :: Num v => Lookup String v
abc "a" = Just 1
abc "b" = Just 2
abc "c" = Just 3
abc _   = Nothing


put :: Eq k => k -> v -> Lookup k v -> Lookup k v
put k v lookup = 
    \key -> if key == k 
            then Just v 
            else lookup key

--
sum :: Num a => [a] -> a
sum []     = 0
sum (x:xs) = x + sum xs

product :: Num a => [a] -> a
product []     = 1
product (x:xs) = x * product xs

map :: (a -> b) -> [a] -> [b]
map _ []     = []
map f (x:xs) = f x : map f xs 

filter :: (a -> Bool) -> [a] -> [a]
filter _ [] = []
filter p (x:xs) = if p x then x : filter p xs else filter p xs

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr fn z []     = z
foldr fn z (x:xs) = fn x y 
    where y = HigherOrder.foldr fn z xs

sum' :: Num a => [a] -> a
sum' = HigherOrder.foldr (+) 0

product' :: Num a => [a] -> a
product' = HigherOrder.foldr (*) 1

map' :: (a -> b) -> [a] -> [b]
map' f = HigherOrder.foldr ((:) . f) []

filter' :: (a -> Bool) -> [a] -> [a]
filter' p = HigherOrder.foldr (\x xs -> if p x then x : xs else xs) []

data Tree a = Leaf
            | Node a (Tree a) (Tree a) deriving (Foldable)

sumTree :: Num a => Tree a -> a
sumTree Leaf = 0
sumTree (Node x l r) = x + sumTree l + sumTree r

productTree :: Num a => Tree a -> a
productTree Leaf = 1
productTree (Node x l r) = x * sumTree l * sumTree r

foldTree :: (a -> b -> b) -> b -> Tree a -> b
foldTree f z Leaf = z
foldTree f z (Node a left right) = foldTree f z' left where
   z'  = f a z''
   z'' = foldTree f z right

sumTree' = foldTree (+) 0
productTree' = foldTree (*) 1

fact = foldr (*) 1 . unfoldr (\n -> if n ==0 then Nothing else Just (n, n-1))


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

    let tree = Node 2 (Node 3 Leaf Leaf) (Node 4 Leaf Leaf)
    print $ sumTree tree
    print $ sumTree' tree
    print $ Prelude.foldr (+) 0 tree

    print $ productTree tree
    print $ productTree' tree
    print $ foldr (*) 1 tree

    print $ unfoldr (\n -> if n==0 then Nothing else Just (n, n-1)) 10




