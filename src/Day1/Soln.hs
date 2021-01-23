{-# LANGUAGE OverloadedStrings #-}

module Day1.Soln where

import Data.Maybe
import Data.List
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Debug.Trace

-- input

readLines :: IO [T.Text]
readLines = T.splitOn "\n" <$> TIO.readFile "src/Day1/short-input.txt"

readExpenses :: IO [Int]
readExpenses = fmap ((read :: String -> Int) . T.unpack) <$> readLines

-- soln 1

desiredTotal :: Int
desiredTotal = 2020

findSummands :: [Int] -> (Int, Int)
findSummands list = let set = S.fromList list 
                        sum2 = head $ filter (\x -> S.member (2020 - x) set) list
                     in (2020 - sum2, sum2)

findFinalExpense :: [Int] -> Int
findFinalExpense expenses = let (e1, e2) = findSummands expenses
                             in e1 * e2

main :: IO ()
main = do expense <- findFinalExpense <$> readExpenses
          print expense
        