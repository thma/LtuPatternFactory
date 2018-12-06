module TemplateMethod where

import           Adapter (Minute (..), WallTime (..), addMinutesToWallTime, marshalMW, unmarshalWM)

addMinutesTemplate :: (Int -> WallTime -> WallTime) -> Int -> Minute -> Minute
addMinutesTemplate f x =
    unmarshalWM .
    f x .
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

addWallTimes :: WallTime -> WallTime -> WallTime
addWallTimes a@(WallTime (h,m)) b =
  let aMin = h*60 + m
  in  addMinutesToWallTime aMin b

instance Semigroup WallTime where
  (<>)   = addWallTimes
instance Monoid WallTime where
  mempty = WallTime (0,0)

templateMethodDemo = do
    putStrLn "TemplateMethod -> higher order function -> typeclass default implementations"
    putStrLn $ "linear time: " ++ (show $ linearTimeAdd 100 (Minute 1400))
    putStrLn $ "cyclic time: " ++ (show $ cyclicTimeAdd 100 (Minute 1400))
    putStrLn ""
    let a = WallTime (3,20)
    print $ mappend a a
    print $ mconcat [a,a,a,a,a,a,a,a,a]
    putStrLn ""
