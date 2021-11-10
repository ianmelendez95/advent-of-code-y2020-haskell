module Day19.Soln where

import Text.Megaparsec
import qualified Data.Text.IO as TIO

import Day19.Parse


testInputParsing :: IO ()
testInputParsing = 
  do input <- TIO.readFile "src/Day19/short-input.txt"
     case parse fullSpec "" input of
       Left bundle -> putStr (errorBundlePretty bundle)
       Right spec -> putStr (showFullSpec spec)

