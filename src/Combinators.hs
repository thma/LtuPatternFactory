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

-- lispkit
data Term =
    Free String         -- free variable
  | Bound String        -- bound variable
  | Int Integer         -- an integer value
  | Op String           -- an operator name
  | Abs (String, Term)  -- a lambda abstraction
  | Apply (Term, Term)  -- an application
  deriving (Show)

-- "(%x y.ADD x y) 3 4"
-- "(%x.ADD x 4) 3"
sumTerm1 = Apply (Abs ("x", Apply (Op "ADD", Apply(Bound "x", Int 4))), Int 3)  
sumTerm2 = Apply (Apply (Abs ("x", Abs ("y", Apply (Op "ADD", Apply(Bound "x", Bound "y")))), Int 3) , Int 4) 

-- abstract a variable from a lambda term  
abstract :: String -> Term -> Term  
abstract x (Free y)      = Apply(Op "K", Free y)
abstract x (Bound y)     = if y==x then Op "I" else Apply(Op "K", Bound y)
abstract x (Int y)       = Apply(Op "K", Int y)
abstract x (Op y)        = Apply(Op "K", Op y)
abstract x (Abs(y,body)) = abstract x (abstract y body)
abstract x (Apply(a,b))  = Apply(Apply(Op "S",abstract x a), abstract x b)

data Snode = 
  Satom Value
 |Scomb Comb
 |Sapp (Snode, Snode) deriving (Show, Eq)

data Comb = S | K | I | B | C | Y | CONS | HD | TL | PLUS | MINUS | TIMES | DIV | IF | EQL | AND | OR | NOT | DEF String deriving (Show, Eq)

data Value = 
  Integer Integer
 | Real Double
 | Bool Bool
 deriving (Show, Eq)

c :: Term -> Snode
c (Free a)      = Scomb(DEF a)
c (Bound a)     = error $ "Variable " ++ a ++ " has not been abstracted"
c (Int a)       = Sapp(Scomb K,Satom(Integer a))
c (Op k)        = Sapp(Scomb K,Scomb(mkComb k))
c (Apply(a,b))  = Sapp(c a,c b)
c (Abs(x,body)) = c (abstract x body);

opt :: Snode -> Snode
opt (Sapp(Sapp(Scomb S,Sapp(Scomb K,e)),Scomb I)) = e
opt (Sapp(Sapp(Scomb S,Sapp(Scomb K,e1)),Sapp(Scomb K,e2))) = Sapp(Scomb K,Sapp(e1,e2))
opt (Sapp(Sapp(Scomb S,Sapp(Scomb K,e1)),e2)) = Sapp(Sapp(Scomb B,e1),e2)
opt (Sapp(Sapp(Scomb S,e1),Sapp(Scomb K,e2))) = Sapp(Sapp(Scomb C,e1),e2)
opt (Sapp(e1,e2)) = Sapp(opt e1,opt e2)
opt x = x;

ropt :: Snode -> Snode
ropt x = 
  let y = opt x;
   in if y==x then x else ropt y

mkComb "I" = I
mkComb "K" = K
mkComb "S" = S
mkComb "B" = B
mkComb "C" = C
mkComb "Y" = Y
mkComb "CONS" = CONS
mkComb "HD" = HD
mkComb "TL" = TL
mkComb "ADD" = PLUS
mkComb "SUB" = MINUS
mkComb "MUL" = TIMES
mkComb "DIV" = DIV
mkComb "IF" = IF
mkComb "EQ" = EQL
mkComb "AND" = AND
mkComb "OR" = OR
mkComb "NOT" = NOT
--mkComb "PR" = PR
mkComb str = DEF str;

combinatorDemo :: IO ()
combinatorDemo = do
  putStrLn "SKI combinators\n"
  print $ s k k 3
  print $ fix facY 100
  putStrLn ""
