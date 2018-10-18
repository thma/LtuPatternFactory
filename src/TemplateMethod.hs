module TemplateMethod where

import Adapter (unmarshalWM, marshalMW, addMinutesToWallTime, Minute (..), WallTime (..) )

addMinutesTemplate :: (Int -> WallTime -> WallTime) -> Int -> Minute -> Minute
addMinutesTemplate tf x =
    unmarshalWM .
    tf x .
    marshalMW

-- implements linear addition even for values > 1440
linearTimeAdd :: Int -> Minute -> Minute
linearTimeAdd = addMinutesTemplate addMinutesToWallTime

-- implements cyclic addition, respecting a 24 hour (1440 Min) cycle
cyclicTimeAdd :: Int -> Minute -> Minute
cyclicTimeAdd = addMinutesTemplate addMinutesToWallTime'


-- a 24 hour (1440 min) cyclic version of addition: 1400 + 100 = 60
addMinutesToWallTime' :: Int -> WallTime -> WallTime
addMinutesToWallTime' x (WallTime (h, m)) =
    let (hAdd, mAdd) = x `quotRem` 60
        hNew = h + hAdd
        mNew = m + mAdd
    in if mNew >= 60
        then WallTime ((hNew + 1) `rem` 24, mNew-60)
        else WallTime (hNew, mNew)

templateMethodDemo = do
    putStrLn "TemplateMethod -> higher order function -> typeclass default implementations"
    putStrLn $ "linear time: " ++ (show $ linearTimeAdd 100 (Minute 1400))
    putStrLn $ "cyclic time: " ++ (show $ cyclicTimeAdd 100 (Minute 1400))
    putStrLn ""
