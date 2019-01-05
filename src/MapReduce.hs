module MapReduce where

import           Control.Arrow    ((&&&))
import           Control.Category ((>>>))
import           Data.Char        (toLower)
import           Data.List        (group, sort)
import           Data.Map         as Map hiding (filter, map, foldr)
import           Control.Parallel (pseq)
import           Control.Parallel.Strategies (rseq, using, parMap)
import           Data.Coerce

newtype WordCountMap = WordCountMap (Map.Map String Int) deriving (Show)

instance Semigroup WordCountMap where
    WordCountMap a <> WordCountMap b = WordCountMap $ Map.unionWith (+) a b
instance Monoid WordCountMap where
    mempty = WordCountMap Map.empty


stringToWordCountMap :: String -> WordCountMap
stringToWordCountMap =
  map toLower >>> words >>>  -- convert to lowercase and split into a list of words
  sort >>> group >>>         -- sort the words and group all equal words to sub-lists
  map (head &&& length) >>>  -- for each of those list of grouped words: form a pair (word, frequency)
  Map.fromList >>>           -- create a Map from the list of (word, frequency) pairs
  WordCountMap               -- wrap as WordCountMap

reduceWordCountMaps :: [WordCountMap] -> WordCountMap
reduceWordCountMaps = WordCountMap . foldr (Map.unionWith (+) . coerce) empty 

simpleMapReduce ::
     (a -> b)   -- map function
  -> ([b] -> c) -- reduce function
  -> [a]        -- list to map over
  -> c          -- result
simpleMapReduce mapFunc reduceFunc = reduceFunc . map mapFunc

parMapReduce :: (a -> b) -> ([b] -> c) -> [a] -> c
parMapReduce mapFunc reduceFunc input =
    mapResult `pseq` reduceResult
    where mapResult    = parMap rseq mapFunc input
          reduceResult = reduceFunc mapResult `using` rseq

alphabetic :: Char -> Bool
alphabetic char = char `elem` (" \t\n\r" ++ ['a'..'z'] ++ ['A'..'Z'])        

mapReduceDemo = do
  contents <- readFile "LICENSE"
  let linesInFile = lines $ filter alphabetic contents

  -- simple map reduce, no parallelism
  print $ simpleMapReduce stringToWordCountMap reduceWordCountMaps linesInFile
  -- parallelized map reduce
  print $ parMapReduce stringToWordCountMap reduceWordCountMaps linesInFile

  -- the essence of map reduce: foldMap:
  print $ foldMap stringToWordCountMap linesInFile

