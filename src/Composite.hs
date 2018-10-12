module Composite where

-- most simple version of a test case: just a string
type TestCase = String

-- the composite data structure: a TestComponent can be Empty, a single TestCase
-- or a TestSuite holding a list of TestComponents
data TestComponent = Empty
                   | Test TestCase
                   | TestSuite [TestComponent] deriving (Show, Eq)

-- in order to make TestComponent an instance of Monoid, we have to provide
-- an operator <> which is required to be associative
-- and a neutral element mempty
instance Semigroup TestComponent where
    (<>) = add
instance Monoid TestComponent where
    mempty = Empty

-- simulate execution of a TestComponent
runTest :: TestComponent -> IO ()
runTest Empty         = return () -- special handling of the neutral element
runTest (Test t)      = putStrLn $ "  " ++ t ++ " ... operational"
runTest (TestSuite l) = do 
    putStrLn $ "TestSuite"
    mapM_ runTest l

-- adding TestComponents
add :: TestComponent -> TestComponent -> TestComponent
add Empty t = t
add t Empty = t
add t1@(Test _) t2@(Test _)       = TestSuite [t1,t2]
add t1@(Test _) (TestSuite list)  = TestSuite ([t1] ++ list)
add (TestSuite list) t2@(Test _)  = TestSuite (list ++ [t2])
add (TestSuite l1) (TestSuite l2) = TestSuite (l1 ++ l2)


compositeDemo = do
    putStrLn "Composite -> Monoid"
    let t1 = Test "Flux capacitator"
    let t2 = Test "Warp drive"
    let t3 = Test "Tractor beam"
    let t4 = Test "Tricorder"
    let ts1 = TestSuite [t1,t2]
    let ts2 = TestSuite [t3,t4]

    runTest $ t1 <> t2 <> t3
    print $ if (t1 <> t2) <> t3 == t1 <> (t2 <> t3)
                then "adding tests is associative"
                else "error in adding tests"
    

    runTest $ mconcat [t1,t2,t3,t4]
    runTest $ mconcat [ts1,ts2]

    runTest $ t1 <> (t2 <> ts2)
    runTest $ (t1 <> t2) <> ts2

    runTest $ ts1 <> (t3 <> t4)
    runTest $ (ts1 <> t3) <> t4

    print $ if (t1 <> Empty == Empty <> t1) && (t1 == Empty <> t1)
              then "Empty is neutral element"
              else "error: Empty is not the neutral element"

