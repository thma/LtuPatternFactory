module MapReduce where

import Data.Char (toLower)
import Data.List (sort, group)
import Control.Arrow ((&&&)) 
import Data.Map as Map hiding (map, filter)
import Data.Monoid

stringToWordCountMap :: String -> Map.Map String Int
stringToWordCountMap  = Map.fromList . map (head &&& length) . group . sort . words . map toLower 

combineWordCountMaps :: Map.Map String Int -> Map.Map String Int -> Map.Map String Int
combineWordCountMaps = Map.unionWith (+)

reduceWordCountMaps :: [Map.Map String Int] -> Map.Map String Int
reduceWordCountMaps [x]    = x
reduceWordCountMaps (x:xs) = combineWordCountMaps x (reduceWordCountMaps xs)

--countWords :: String -> Map.Map String Int
--countWords = 

simpleMapReduce
    :: (a -> b)      -- map function
    -> ([b] -> c)    -- reduce function
    -> [a]           -- list to map over
    -> c             -- result
simpleMapReduce mapFunc reduceFunc  = reduceFunc . Prelude.map mapFunc  

filterBy :: String -> Char -> Bool
filterBy notAllowedChars char = 
     char `notElem` notAllowedChars

mapReduceDemo = do
    contents <- readFile "LICENSE"
    let linesInFile = lines $ filter (filterBy ".,;:!?()[]{}\"'") contents
    let result = simpleMapReduce stringToWordCountMap reduceWordCountMaps linesInFile
    putStrLn $ "The file has " ++ show (length (lines contents)) ++ " lines!"
    putStrLn $ "result = " ++ show result ++ "."
