{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Day17.Soln where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Data.Set (Set)
import qualified Data.Set as Set

import Debug.Trace

type Point = (Int, Int, Int)
type Universe = Set Point

readUniverse :: IO Universe
readUniverse = do lines <- map T.unpack . T.splitOn "\n" <$> TIO.readFile "src/Day17/short-input.txt"
                  return $ Set.fromList (readLines lines)
  where 
    -- | returns active y coordinates
    readLine :: String -> [Int]
    readLine line = let indexed = zip [0..] line
                        hashes = filter (\(_, c) -> c == '#') indexed
                     in map fst hashes

    readLines :: [String] -> [Point]
    readLines lines = let ycoords :: [[Int]]
                          ycoords = map readLine lines
                          
                          xycoords :: [(Int,Int)]
                          xycoords = concat $ zipWith (\x ys -> map (x,) ys) [0..] ycoords
                       in map (\(x,y) -> (x,y,0)) xycoords