module Adapter where 

backend :: c -> d
backend = undefined
    
marshal :: a -> c
marshal = undefined

unmarshal :: d -> b
unmarshal = undefined

adapter :: a -> b
adapter = unmarshal . backend . marshal

-- a 24:00 hour clock representation of time 
newtype WallTime = WallTime (Int, Int) deriving (Show)

-- this is our backend. It can add minutes to a WallTime representation
addMinutesToWallTime :: Int -> WallTime -> WallTime
addMinutesToWallTime x (WallTime (h, m)) =
    let (hAdd, mAdd) = x `quotRem` 60
        hNew = h + hAdd
        mNew = m + mAdd
    in if mNew >= 60
        then
            let (dnew, hnew') = (hNew + 1) `quotRem` 24
            in  WallTime (24*dnew + hnew', mNew-60)
        else WallTime (hNew, mNew)

-- this is our time representation in Minutes that we want to use in the frontend
newtype Minute = Minute Int deriving (Show)

-- convert a Minute value into a WallTime representation
marshalMW :: Minute -> WallTime
marshalMW (Minute x) = 
    let (h,m) = x `quotRem` 60
    in WallTime (h `rem` 24, m)

-- convert a WallTime value back to Minutes
unmarshalWM :: WallTime -> Minute
unmarshalWM (WallTime (h,m)) = Minute $ 60 * h + m

-- this is our frontend that add Minutes to a time of a day 
-- measured in minutes
addMinutesAdapter :: Int -> Minute -> Minute
addMinutesAdapter x = unmarshalWM . addMinutesToWallTime x . marshalMW

adapterDemo = do 
    putStrLn "Adapter -> function composition"
    print $ addMinutesAdapter 100 $ Minute 400
    putStrLn ""