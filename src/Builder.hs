module Builder where

-- accountNo, Name, branch, balance, interestRate    
data BankAccount = BankAccount {
    accountNo    :: Int
  , name         :: String
  , branch       :: String
  , balance      :: Double
  , interestRate :: Double
} deriving (Show)

defaultAccount :: Int -> BankAccount
defaultAccount i = BankAccount i "" "" 0 0

builderDemo = do
    putStrLn "Builder -> ???"
    let acc1 = BankAccount {
                  accountNo = 1234
                , name = "Marjin"
                , branch = "Reikjavik"
                , balance = 1000
                , interestRate = 2.5
                }
    print acc1
    let acc2 = defaultAccount 4711
    print acc2
    let acc3 = acc2 {name="Hans Mejer", branch="London", balance=10000}
    print acc3
    