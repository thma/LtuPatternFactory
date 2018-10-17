module Builder where

-- accountNo, Name, branch, balance, interestRate    
data BankAccount = BankAccount {
    accountNo    :: Int
  , name         :: String
  , branch       :: String
  , balance      :: Double
  , interestRate :: Double
} deriving (Show)

builder :: Int -> ((Int, String) -> (Int, String, String))
builder acc name = withName (acc, name)

withName = undefined

builderDemo = do
    putStrLn "Builder -> ???"
    let acc1 = BankAccount {
                  accountNo = 1234
                , name = "Marjin"
                , branch = "Reikjavik"
                , balance = 1000
                , interestRate = 2.5
                } 
    putStrLn "build an account"
    print acc1