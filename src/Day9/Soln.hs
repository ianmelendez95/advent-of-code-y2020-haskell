{-# LANGUAGE OverloadedStrings #-}

module Day9.Soln where

import qualified Data.Text as T
import Data.Text.Read
import qualified Data.Text.IO as TIO
import Data.Either

import qualified Data.Set as S

import GHC.Exts (IsList (..))
import Deque.Strict (Deque)
import qualified Deque.Strict as Deq

import Debug.Trace

-- solutions

soln :: IO Int 
soln = do xmas <- readXMAS
          let corrupt = findCorruptNum xmas
              contiguous = contiguousSum corrupt xmas
          return (minimum contiguous + maximum contiguous)

-- input

readXMAS :: IO [Int]
readXMAS = do lines <- T.splitOn "\n" <$> TIO.readFile inputFile
              return $ map readInt lines

readInt :: T.Text -> Int
readInt text = case decimal text of 
                 Left err -> error err 
                 Right (res, "") -> res

-- input params

type Input = (String, Int)

input :: Input 
input = ("src/Day9/full-input.txt", 25)
-- input = ("src/Day9/short-input.txt", 5)

inputFile :: FilePath
inputFile = fst input

preamble :: Int
preamble = snd input

-- contiguous sum 

-- | (sum, summands) - summands first in front (cons), last in back (snoc)
type ContigSum = (Int, Deque Int) 

newContigSum :: ContigSum
newContigSum = (0, fromList [])

pushSummand :: Int -> ContigSum -> ContigSum
pushSummand summand (csum, csummands) 
  = (csum + summand, Deq.cons (trace ("Pushing summand: " ++ show summand) summand) csummands)

popSummand :: ContigSum -> ContigSum
popSummand (csum, csummands) = case Deq.unsnoc csummands of 
                                 Nothing -> error "no summands left"
                                 Just (x, newcsummands) -> (csum - trace ("popped summand: " ++ show x) x, newcsummands)

contiguousSum :: Int -> [Int] -> [Int]
contiguousSum sum xmas = let deque = fromList [] :: Deque Int
                             contig = doContigSum newContigSum xmas
                          in toList (snd contig)
  where
    doContigSum :: ContigSum -> [Int] -> ContigSum
    doContigSum c_sum@(csum, csummands) (x : xs) 
      = case compare sum (csum + x) of 
          EQ -> pushSummand x c_sum
          LT -> doContigSum (popSummand c_sum) (x:xs)
          GT -> doContigSum (pushSummand x c_sum) xs

-- find corrupt

findCorruptNum :: [Int] -> Int
findCorruptNum xmas = let (pre, rest) = splitAt preamble xmas 
                       in find pre rest
  where 
    find :: [Int] -> [Int] -> Int
    find psl@(p : ps) (x : xs) = if hasSummands x psl
                                    then find (ps ++ [x]) xs
                                    else x

hasSummands :: Int -> [Int] -> Bool
hasSummands sum list = let set = S.fromList list 
                           s2s = filter (\s1 -> S.member (sum - s1) set) list
                        in not $ null s2s