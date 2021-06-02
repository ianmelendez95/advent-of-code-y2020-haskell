module Day15.Soln where 

import Data.Maybe (fromMaybe)
import qualified Data.Map as Map
import Control.Monad.State.Lazy

--------------------------------------------------------------------------------
-- Solution

soln :: IO Int
soln = iterNums _iters <$> readInput

soln' :: Int -> IO Int
soln' iters = iterNums iters <$> readInput

_iters :: Int 
_iters = 2020

--------------------------------------------------------------------------------
-- IO

inputFile :: FilePath 
inputFile = "src/Day15/full-input.txt"

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

iterations :: Int 
iterations = 2020

--------------------------------------------------------------------------------
-- NumState

type NumToOccurence = Map.Map Int Int

iterNums :: Int -> [Int] -> Int
iterNums iters initial_nums = iterNums' iters ntocc last_n cur_iter
  where 
    ntocc    = Map.fromList (zip initial_nums [1..])
    last_n   = last initial_nums
    cur_iter = length initial_nums + 1

iterNums' :: Int -> NumToOccurence -> Int -> Int -> Int
iterNums' iters ntocc last_n cur_iter 
  | cur_iter > iters = last_n
  | otherwise = 
      let last_per = 
            maybe 0 ((cur_iter - 1) -) $ Map.lookup last_n ntocc
       in iterNums' iters 
                    (Map.insert last_n (cur_iter - 1) ntocc)
                    last_per
                    (cur_iter + 1)
