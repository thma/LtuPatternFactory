module Composite where

type TestCase = String

data TestComponent = Test TestCase
                   | TestSuite (String, [TestComponent]) deriving (Show)


instance Semigroup TestComponent where
    (<>) = addTest

instance Monoid TestComponent where
    mempty = Test ""

runTest :: TestComponent -> IO ()
runTest = performAllTests 0 

performAllTests :: Int -> TestComponent -> IO ()
performAllTests _ (Test "") = return ()
performAllTests n (Test t)  = putStrLn $ (replicate n ' ') ++ t ++ " ...operational"
performAllTests n (TestSuite (title , list)) = do 
    putStrLn $ (replicate n ' ') ++ title
    mapM_ (performAllTests (n+2)) list

addTest :: TestComponent -> TestComponent -> TestComponent
addTest (Test "") t = t
addTest t (Test "") = t
addTest t1@(Test _) t2 = TestSuite ("TestSuite", [t1,t2])
addTest (TestSuite (title, list)) t = TestSuite (title, list ++ [t])

compositeDemo = do
    putStrLn "Composite -> Monoid"
    let t1 = Test "Flux capacitator"
    let t2 = Test "Warp drive"
    let t3 = Test "Ansible"
    let t4 = Test "Tractor beam"
    let t5 = Test "Tricorder"
    let ts1 = TestSuite ("Ship", [t1,t2])
    let ts2 = TestSuite ("Fleet", [t3,t4,t5])
    let ts3 = t3 <> t4
    let ts4 = mconcat [t1,t2,ts1,ts2]
    runTest ts3
    runTest $ (t1 <> t2) <> t3
    --runTest $ t1 <> t2 <> t3
    --runTest $ mconcat [ts1, ts2]