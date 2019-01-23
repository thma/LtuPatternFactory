module Combinators where
import Unsafe.Coerce (unsafeCoerce)
k :: a -> b -> a
k x _ = x

s :: (a -> b -> c) -> (a -> b) -> a -> c
s x y z = x z (y z)

i :: a -> a
i x = x 
--i = s k k

fix :: (a -> a) -> a
fix f = f (fix f)

newtype Fn a = Fn (Fn a -> a)
y f = (\h -> h $ Fn h) (\x -> f . (\(Fn g) -> g) x $ x)

y' :: (a -> a) -> a
y' f = (\x -> f (unsafeCoerce x x)) (\x -> f (unsafeCoerce x x))


fac :: Integer -> Integer
fac = fix facY

facY f n = if n <= 0 then 1 else n * f (n - 1)

combinatorDemo :: IO ()
combinatorDemo = do
  putStrLn "SKI combinators\n"
  print $ s k k 3
  print $ fix facY 100
  putStrLn ""
