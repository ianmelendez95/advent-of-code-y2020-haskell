{-# LANGUAGE OverloadedStrings #-}

module Y2019.Day2.Soln where

import Data.Array
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Debug.Trace

readCodes :: IO (Array Int Int)
readCodes = do codestrs <- T.splitOn (T.pack ",") <$> TIO.readFile "src/Day2/full-input.txt"
               let codeints = map ((read :: String -> Int) . T.unpack) codestrs
               return $ listArray (0, length codeints - 1) codeints

runComputer :: Int -> Array Int Int -> Array Int Int 
runComputer startIndex array = let res@(opcode, op1, op2, target) = (array ! startIndex, 
                                                                     array ! (startIndex + 1), 
                                                                     array ! (startIndex + 2), 
                                                                     array ! (startIndex + 3))
                               in  case trace ("result: " ++ show res) opcode of 
                                     99 -> array
                                     1  -> runComputer (startIndex + 4) (array // [(target, (array ! op1) + (array ! op2))])
                                     2  -> runComputer (startIndex + 4) (array // [(target, (array ! op1) * (array ! op2))])
                                     _  -> error $ "Unknown case: " ++ show opcode

restoreGravityAssist :: Array Int Int -> Int
restoreGravityAssist codes = runComputer 0 (codes // [(1, 12), (2, 2)]) ! 0

main :: IO ()
main = readCodes >>= print