--{-# OPTIONS -fglasgow-exts #-}
{-# LANGUAGE UndecidableInstances, ExistentialQuantification, FunctionalDependencies, Rank2Types, ScopedTypeVariables, MultiParamTypeClasses, FlexibleInstances #-}
module Diff where

import Prelude hiding ((+), (-), (*), (/), (^), sin, cos, fromInteger)
import qualified Prelude

class D a where
    (+):: a -> a -> a
    (*):: a -> a -> a
    (-):: a -> a -> a
    (/):: a -> a -> a
    (^):: a -> Int -> a
    sin:: a -> a
    cos:: a -> a
    fromInteger:: Integer -> a

instance D Float where
    (+) = (Prelude.+)
    (-) = (Prelude.-)
    (*) = (Prelude.*)
    (/) = (Prelude./)
    (^) = (Prelude.^)
    sin = Prelude.sin
    cos = Prelude.cos
    fromInteger = Prelude.fromInteger

-- Here, reflect is the tag eliminator -- or `compiler'
class Term t a | t -> a where
    reflect :: t -> a -> a

--We should point out that the terms are fully typeful.

newtype Const a = Const a deriving Show
data Var a   = Var        deriving Show
data Add x y = Add x y    deriving Show
data Sub x y = Sub x y    deriving Show
data Mul x y = Mul x y    deriving Show
data Div x y = Div x y    deriving Show
data Pow x   = Pow x Int  deriving Show
newtype Sin x   = Sin x   deriving Show
newtype Cos x   = Cos x   deriving Show

--We can now describe the grammar of our term representation in the
--following straightforward way: 

instance Term (Const a) a where reflect (Const a) = const a

instance Term (Var a) a where reflect _ = id

instance (D a, Term x a, Term y a) => Term (Add x y) a where
    reflect (Add x y) = \a -> (reflect x a) + (reflect y a)

instance (D a, Term x a) => Term (Sin x) a where
    reflect (Sin x) = sin . reflect x 

{--
The other instances are given in the Appendix. This is the straightforward
emulation of GADT. The function `reflect' removes the `tags' after the
symbolic differentiation. Actually, `Sin' is a newtype constructor, so
there is no run-time tag to eliminate in this case. 

We must stress that there is no `reify' function. One may say it is
built into Haskell already.

We only need to declare the datatype for the reified code
--}

data Code a = forall t. (Show t, Term t a, DiffRules t a) => Code t
instance Show a => Show (Code a) where 
    show (Code t) = show t

reflect_code (Code c) = reflect c


--inject the reified code in the D domain

instance D a => D (Code a) where
    Code x + Code y = Code $ Add x y
    Code x - Code y = Code (Sub x y)
    Code x * Code y = Code $ Mul x y
--    (Code x) / (Code y) = Code $ Div x y
--    (Code x) ^ n    = Code $ Pow x n
--    sin (Code x)    = Code $ Sin x
--    cos (Code x)    = Code $ Cos x
--    fromInteger n   = Code $ Const $ fromInteger n

-- We can define a function

test1f x = x * x + fromInteger 1
test1 = test1f (2.0::Float)

-- we can even compile it. At any point, we can reify it

test1c = test1f (Code Var :: Code Float)

-- and reflect it back:

test1fr = reflect_code test1c
test1r = test1fr (2.0::Float)

{--
    *Difftest1
    5.0
    *Difftest1r
    5.0
    *Difftest1c
    Add (Mul Var Var) (Const 1.0)

The differentiation part is quite straightforward. We declare a class
for differentiation rules
--}

class (Term t a,D a) => DiffRules t a | t -> a where 
    diff :: t -> Code a

--The rules are the instances of the class DiffRules

instance D a => DiffRules (Const a) a where
    diff _ = Code $ Const $ fromInteger 0

instance D a => DiffRules (Var a) a where
    diff _ = Code $ Const $ fromInteger 1

instance (Show x, Show y, DiffRules x a, DiffRules y a) => DiffRules (Mul x y) a where
    diff (Mul x y) = case (diff x,diff y) of
               (Code x'::Code a,Code y') ->
                Code $ Add (Mul (x::x) y') (Mul x' (y::y))

instance (Show x, Show a, DiffRules x a) => DiffRules (Sin x) a where
    diff (Sin x) = case diff x of
               (Code x'::Code a) ->
                Code $ Mul x' (Cos x)

instance (D a, Term x a, Term y a) => Term (Sub x y) a where
    reflect (Sub x y) = \a -> (reflect x a) - (reflect y a)

instance (D a, Term x a, Term y a) => Term (Mul x y) a where
    reflect (Mul x y) = \a -> (reflect x a) * (reflect y a)

instance (D a, Term x a, Term y a) => Term (Div x y) a where
    reflect (Div x y) = \a -> (reflect x a) / (reflect y a)

instance (D a, Term x a) => Term (Pow x) a where
    reflect (Pow x n) = (^ n) . reflect x

instance (D a, Term x a) => Term (Cos x) a where
    reflect (Cos x) = cos . reflect x 

instance (Show x, Show y, DiffRules x a, DiffRules y a) => DiffRules (Add x y) a where
    diff (Add x y) = case (diff x,diff y) of
                (Code x'::Code a,Code y') -> Code $ Add x' y'

instance (Show x, Show y, Show a, DiffRules x a, DiffRules y a) => DiffRules (Sub x y) a where
    diff (Sub x y) = case (diff x,diff y) of
                (Code x'::Code a,Code y') -> Code $ Sub x' y'

instance (Show x, Show y, Show a, DiffRules x a, DiffRules y a) => DiffRules (Div x y) a where
    diff (Div x y) = case (diff x,diff y) of
                (Code x'::Code a,Code y') -> Code $ 
                    Div (Sub (Mul x' y) (Mul x y'))
                        (Pow y 2)

instance (Show x, Show a, DiffRules x a) => DiffRules (Pow x) a where
    diff (Pow x n) = case diff x of
                (Code x'::Code a) ->
                    Code $ Mul (Const (fromInteger $ toInteger n))
                            (Mul x' (Pow x (n Prelude.- 1)))

instance (Show x, Show a, DiffRules x a) => DiffRules (Cos x) a where
    diff (Cos x) = case diff x of
                (Code x'::Code a) ->
                    Code $ Mul x' (Sub (Const $ fromInteger 0) (Sin x))
                

diff_code (Code c) = diff c

--the differentiation operator could not be any simpler.

diff_fn :: D b => (forall a. D a => a -> a) -> b -> b
diff_fn f = 
    let code = f (Code Var)
    in reflect_code $ diff_code code


--We can try 

test1f' x = diff_fn test1f x
test1' = test1f' (3.0::Float) -- 6.0

--we can even see the differentiation result, symbolically:
{--
    *Diff> diff_code test1c
    Add (Add (Mul Var (Const 1.0)) (Mul (Const 1.0) Var)) (Const 0.0)
--}

--The function test1f' can be differentiated further

test1f'' x = diff_fn test1f' x
test1'' = test1f'' (10.0::Float) -- 2.0

{--
True, simplifications are direly needed. Well, the full computer
algebra system is a little bit too big to be developed over one
evening. Besides, I wanted to go home three hours ago.

Here's a slightly more complex example:
--}
test5f x = sin (fromInteger 5*x) + cos(fromInteger 1 / x)
test5c = test5f (Code Var :: Code Float)

test5 = test5f (pi::Float)
test5d = diff_code test5c

test6 = diff_fn test5f (pi::Float)

{--
One can evaluate the function test5f numerically, differentiate it
symbolically, check the result of differentiation -- and evaluate it
numerically right away.

We can even do partial derivatives:
--}

test3f x y = (x*y + ((fromInteger 5)*(x^2))) / y

test3c1 = test3f (Code Var :: Code Float) (fromInteger 10)

test4x y = diff_fn (\x -> test3f x (fromInteger y))
test4y x = diff_fn (test3f (fromInteger x))

-- *Difftest4x 1 (2::Float) -- partial derivative with respect to x
-- 21.0
-- *Difftest4y 5 (5::Float) -- partial derivative with respect to y
-- -5.0


