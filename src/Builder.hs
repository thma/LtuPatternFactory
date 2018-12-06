module Builder where

-- accountNo, Name, branch, balance, interestRate
data BankAccount = BankAccount {
    accountNo    :: Int
  , name         :: String
  , branch       :: String
  , balance      :: Double
  , interestRate :: Double
} deriving (Show)

buildAccount :: Int -> BankAccount
buildAccount i = BankAccount i "Dummy Customer" "London" 0 0

builderDemo = do
    putStrLn "Builder -> record syntax, smart constructor"
    let account = buildAccount 1234
    print account
    let account1 = account {name="Marjin Mejer", branch="Paris", balance=10000, interestRate=2}
    print account1

    let account2 = BankAccount {
          accountNo = 5678
        , name = "Marjin Mejer"
        , branch = "Reikjavik"
        , balance = 1000
        , interestRate = 2.5
        }
    print account2

