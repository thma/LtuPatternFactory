module Strategy where

data CustomerType = EndCustomer | Retailer | NGO

type Price = Double
type Quantity = Double

consumerPrice :: Quantity -> Price -> Price
consumerPrice quantity price =
  if quantity <= 3
    then price
    else price * 0.9

retailPrice :: Quantity -> Price -> Price
retailPrice quantity price
  | quantity * price < 100 = price * 0.8
  | quantity * price < 250 = price * 0.7
  | otherwise              = price * 0.5

discountPrice :: CustomerType -> (Quantity -> Price -> Price)
discountPrice EndCustomer = consumerPrice
discountPrice Retailer    = retailPrice
discountPrice NGO         = retailPrice

class CustomerClass a where
  discount :: a -> (Quantity -> Price -> Price)
  
data EndCustomerType = EcType
data RetailerType    = RtType
data NgoType         = NgType

instance CustomerClass EndCustomerType where
  discount _ = consumerPrice

instance CustomerClass RetailerType where
  discount _ = retailPrice  
  
instance CustomerClass NgoType where
  discount _ = retailPrice    
    

strategyDemo = do
    putStrLn "Strategy Pattern -> Higher Order Functions"

    print $ discountPrice EndCustomer 2 10
    print $ discountPrice EndCustomer 9 10

    print $ discountPrice Retailer 9 10
    print $ discountPrice Retailer 20 10
    print $ discountPrice Retailer 60 10
    
    print $ discount EcType 2 10
    print $ discount EcType 9 10
    print $ discount RtType 20 10
    print $ discount NgType 60 10

    putStrLn ""
