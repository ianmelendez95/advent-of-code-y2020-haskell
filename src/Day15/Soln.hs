module Day15.Soln where 

import Data.Maybe (fromMaybe)
import qualified Data.Map as Map
import Control.Monad.State.Lazy

--------------------------------------------------------------------------------
-- Solution

soln :: IO Int
soln = evalNumState . soln' <$> readInput

soln' :: [Int] -> NumState Int
soln' [] = error "Ran out of input"
soln' [x]    = pushNum x
soln' (x:xs) = pushNum x >> soln' xs

--------------------------------------------------------------------------------
-- IO

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

iterations :: Int 
iterations = 2020

--------------------------------------------------------------------------------
-- NumState

type NumToOccurence = Map.Map Int Int
data NumEnv = NEnv {
  nenvCurIdx :: Int, 
  nenvNumToOcc :: NumToOccurence 
}

type NumState = State NumEnv

data NumResult = NewNum
               | RepeatNum Int

evalNumState :: NumState a -> a
evalNumState st = evalState st emptyContext

getLastOccurence :: Int -> NumState (Maybe Int)
getLastOccurence n = Map.lookup n <$> gets nenvNumToOcc

pushOccurence :: Int -> NumState ()
pushOccurence n = 
  do idx <- getAndIncIndex
     modify (\nenv -> nenv { nenvNumToOcc = Map.insert n idx $ nenvNumToOcc nenv })
  where 
    getAndIncIndex :: NumState Int
    getAndIncIndex = 
      do idx <- gets nenvCurIdx
         modify (\nenv -> nenv { nenvCurIdx = idx + 1 })
         pure idx

emptyContext :: NumEnv 
emptyContext = NEnv 0 Map.empty

pushNum :: Int -> NumState Int
pushNum n = 
  do last_occ <- fromMaybe n <$> getLastOccurence n
     let next_val = n - last_occ
     pushOccurence n
     pure next_val
