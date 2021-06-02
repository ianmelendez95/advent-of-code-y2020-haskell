module Day15.Soln where 

import qualified Data.Map as Map

inputFile :: FilePath 
inputFile = "src/Day15/short-input.txt"

readInput :: IO [Int]
readInput = map read . readCSV <$> readFile inputFile

readCSV :: String -> [String]
readCSV [] = []
readCSV input = 
  let (item, rest) = span (/= ',') input
   in item : readCSV (dropWhile (== ',') rest)

--------------------------------------------------------------------------------
-- Rules

{-
0 -> last one was a new number 
N -> period between last number and previous instance
-}

type NumToOccurence = Map.Map Int Int
data NumContext = NC Int NumToOccurence -- (current index, num to last index)

initNumContext :: [Int] -> NumContext
initNumContext ns = undefined