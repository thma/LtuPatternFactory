module Composite where
import Data.Semigroup (All(..))

-- the composite data structure: a Test can be Empty, a single TestCase
-- or a TestSuite holding a list of Tests
data Test = Empty
          | TestCase TestCase
          | TestSuite [Test]

-- a test case produces a boolean when executed
type TestCase = () -> Bool


-- execution of a Test. 
run :: Test -> Bool
run Empty         = True
run (TestCase t)  = t () -- evaluating the TestCase by applying t to ()
--run (TestSuite l) = foldr ((&&) . run) True l
run (TestSuite l) = all (True ==) (map run l) -- running all tests in l and return True if all tests pass


-- addTesting Tests
addTest :: Test -> Test -> Test
addTest Empty t                           = t
addTest t Empty                           = t
addTest t1@(TestCase _) t2@(TestCase _)   = TestSuite [t1,t2]
addTest t1@(TestCase _) (TestSuite list)  = TestSuite ([t1] ++ list)
addTest (TestSuite list) t2@(TestCase _)  = TestSuite (list ++ [t2])
addTest (TestSuite l1) (TestSuite l2)     = TestSuite (l1 ++ l2)


-- in order to make Test an instance of Monoid, we have to provide
-- an operator <> which is required to be associative
-- and a neutral element mempty
instance Semigroup Test where
    (<>) = addTest
instance Monoid Test where
    mempty = Empty

-- a few most simple test cases    
t1 :: Test    
t1 = TestCase (\() -> True)
t2 :: Test 
t2 = TestCase (\() -> True)
t3 :: Test 
t3 = TestCase (\() -> False)
-- collecting all test cases in a TestSuite
ts = TestSuite [t1,t2,t3]


type SmartTestCase = () -> All

tc1 :: SmartTestCase
tc1 () = All True
tc2 :: SmartTestCase
tc2 () = All True
tc3 :: SmartTestCase
tc3 () = All False


compositeDemo = do
    putStrLn "Composite -> SemiGroup -> Monoid"

    print $ run $ t1 <> t2
    print $ run $ t1 <> t2 <> t3

    print $ run $ mconcat [t1,t2]
    print $ run $ mconcat [t1,t2,t3]

    print $ getAll $ mconcat [tc1,tc2] ()
    print $ getAll $ mconcat [tc1,tc2,tc3] ()
