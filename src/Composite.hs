module Composite where

type Test = String

data TestComponent = 
    Empty
  | TestLeaf Test
  | TestComposite (String, [TestComponent]) deriving (Show)

instance Semigroup TestComponent where
    (<>) = addTest

instance Monoid TestComponent where
    mempty = Empty

performAllTests :: TestComponent -> [String]
performAllTests Empty = []
performAllTests (TestLeaf t) = [t]
performAllTests (TestComposite (title , c:cs)) = [title ++ " ["] ++ performAllTests c ++ concatMap performAllTests cs ++ ["]"]

addTest :: TestComponent -> TestComponent -> TestComponent
addTest Empty t = t
addTest t1@(TestLeaf _) t2 = TestComposite ("autonamed", [t1,t2])
addTest (TestComposite (title, list)) t = TestComposite (title, list ++ [t])

compositeDemo = do
    putStrLn "Composite -> Monoid"
    let t1 = TestLeaf "Fluxcapacitator working"
    let t2 = TestLeaf "Warp Kernel operating"
    let t3 = TestComposite ("Simple", [t1,t2])
    let t4 = TestComposite ("Bigger", [t3,t1,t2])
    let t5 = addTest t3 t4
    print $ performAllTests t5