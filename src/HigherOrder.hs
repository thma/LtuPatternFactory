module HigherOrder where

import Data.Ord
import Data.List
import GHC.Exts

type Rect = (Double, Double)

len :: Rect -> Double
len = fst

width :: Rect -> Double
width = snd

size :: Rect -> Double
size rect = width rect * len rect

weightedLength :: Rect -> Double
weightedLength (len, wid) = len^2 * wid

weightedWidth :: Rect -> Double
weightedWidth  (len, wid) = len * wid^2

arrange :: Double -> [Rect] -> [[Rect]]
arrange _        []    = [[]]
arrange maxWidth rects = let (rest, row) = fillRow maxWidth (rects, [])
                          in if null rest 
                                then [row]
                                else row : arrange maxWidth rest

fillRow :: Double -> ([Rect], [Rect]) -> ([Rect], [Rect])
fillRow _ current@([], _) = current
fillRow maxWidth current@(x:xs, row) = 
    if (width x) + (totalWidth row) <= maxWidth
    then fillRow maxWidth (xs, row ++ [x])
    else current

totalWidth :: [Rect] -> Double
totalWidth = foldr ((+) . width) 0

totalLength :: [Rect] -> Double
totalLength = foldr ((+) . len) 0

rects :: [Rect]
rects = [(100,60),(120,60),(80,40),(120,40)]

type WeightFunction = Rect -> Double

arrangeWith :: WeightFunction -> Double-> [Rect] -> [[Rect]]
arrangeWith weightFun maxWidth rects =
    let preorderedRects = sortOn (Down . weightFun) rects
     in arrange maxWidth preorderedRects

weightFunctions = [len,width,size,weightedLength,weightedWidth]

--arrangeWithAll :: [WeightFunction] -> Double -> [Rect] -> [[Rect]]   
arrangeWithAll allFuns maxWidth rects =
    let allTrials = map (\f -> arrangeWith f maxWidth rects) allFuns 
     in map (maximum . map totalLength) allTrials