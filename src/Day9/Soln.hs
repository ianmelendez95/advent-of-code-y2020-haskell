{-# LANGUAGE OverloadedStrings #-}

module Day9.Soln where

import qualified Data.Text as T
import Data.Text.Read
import qualified Data.Text.IO as TIO
import Data.Either

import qualified Data.Set as S

import Debug.Trace

import Data.Sequence (Seq((:<|)))
import qualified Data.Sequence as Seq

-- solutions

soln :: IO Int 
soln = findCorruptNum <$> readXMAS

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

inputFile :: FilePath
inputFile = fst input

preamble :: Int
preamble = snd input

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