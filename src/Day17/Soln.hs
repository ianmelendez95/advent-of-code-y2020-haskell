{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Day17.Soln where

import Data.List
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Set (Set)
import qualified Data.Set as Set

import Data.Sort
import Data.Ord

import Debug.Trace

type Point = (Int, Int, Int, Int)
type Universe = Set Point
type Bounds = ((Int,Int,Int,Int),(Int,Int,Int,Int))

-- solution

soln :: IO Int
soln = Set.size . cycleUniverse 6 <$> readUniverse

inputFile :: FilePath
inputFile = "src/Day17/full-input.txt"

-- IO helpers

printUniverse :: Universe -> IO ()
printUniverse uni = putStrLn (showUniverse uni)

doUniverse :: (Universe -> IO ()) -> IO ()
doUniverse f = readUniverse >>= f

-- read input

readUniverse :: IO Universe
readUniverse = do lines <- map T.unpack . T.splitOn "\n" <$> TIO.readFile inputFile
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
                       in map (\(x,y) -> (x,y,0,0)) xycoords

-- show

showUniverse :: Universe -> String
showUniverse universe = showWs cubePoints
  where
    showPoint' :: Point -> Char
    showPoint' = showPoint universe 

    showYPoints :: Int -> Int -> Int -> [Int] -> String 
    showYPoints w z x ys = map (\y -> showPoint' (x,y,z,w)) ys

    showXVector :: Int -> Int -> [(Int,[Int])] -> String 
    showXVector w z xys = unlines $ map (uncurry (showYPoints w z)) xys

    showZLayer :: Int -> (Int,[(Int,[Int])]) -> String 
    showZLayer w (z,xys) = "Z=" ++ show z ++ " W=" ++ show w ++ "\n" ++ showXVector w z xys ++ "\n"

    showWLayer :: (Int,[(Int,[(Int,[Int])])]) -> String 
    showWLayer (w, zxys) = unlines $ map (showZLayer w) zxys

    showWs :: [(Int,[(Int,[(Int,[Int])])])] -> String
    showWs = unlines . map showWLayer

    -- | cube points structured for printing 
    -- |   e.g. each top level tuple is z to the relative xy points
    -- |        and second level tuples are x to relevant ys
    -- |        thus the top level tuple contains information for the next set of lines
    -- |        and each second level tuple contains information for the given line
    cubePoints :: [(Int,[(Int,[(Int,[Int])])])]
    cubePoints = let ((minx, miny, minz, minw), (maxx, maxy, maxz, maxw)) = universeBounds universe
                     wPoints = [minw..maxw]
                     zPoints = [minz..maxz]
                     yPoints = [miny..maxy]
                     xPoints = [minx..maxx]
                     xyPoints = map (,yPoints) xPoints
                     zxyPoints = map (,xyPoints) zPoints
                     wzxyPoints = map (,zxyPoints) wPoints
                  in wzxyPoints


pointActive :: Universe -> Point -> Bool
pointActive = flip Set.member

showPoint :: Universe -> Point -> Char
showPoint univ point = if pointActive univ point then '#' else '.'

universeBounds :: Universe -> Bounds
universeBounds universe = Set.foldr updateBounds initial universe
  where 
    initial :: Bounds 
    initial = let p = Set.findMin universe in (p, p)

    updateBounds :: Point -> Bounds -> Bounds
    updateBounds (x,y,z,w) ((minx,miny,minz,minw),(maxx,maxy,maxz,maxw)) = 
      ((min x minx, min y miny, min z minz, min w minw), (max x maxx, max y maxy, max z maxz, max w maxw))

-- cycle universe

cycleUniverse :: Int -> Universe -> Universe
cycleUniverse cycles start_universe = iterate nextUniverse start_universe !! cycles

neighbors :: Point -> [Point]
neighbors (x,y,z,w) = [(x',y',z',w') | x' <- [(x-1)..(x+1)],
                                       y' <- [(y-1)..(y+1)],
                                       z' <- [(z-1)..(z+1)],
                                       w' <- [(w-1)..(w+1)],
                                       (x /= x') || (y /= y') || (z /= z') || (w /= w')]

-- | 'Region' about a point is the points and all neighboring points
-- | region of a universe is then all universe points and neighboring points therein
nextPossibleRegion :: Universe -> Universe
nextPossibleRegion = foldr insertRegion Set.empty
  where 
    insertRegion :: Point -> Universe -> Universe 
    insertRegion p = Set.union (Set.fromList (p : neighbors p)) 

pointSurvives :: Universe -> Point -> Bool
pointSurvives universe point 
  | pointActive universe point = activeNeighbors == 2 || activeNeighbors == 3
  | otherwise                  = activeNeighbors == 3 
  where 
    activeNeighbors = length $ filter (pointActive universe) (neighbors point)

nextUniverse :: Universe -> Universe 
nextUniverse universe = let nextRegion = nextPossibleRegion universe 
                         in Set.filter (pointSurvives universe) nextRegion
