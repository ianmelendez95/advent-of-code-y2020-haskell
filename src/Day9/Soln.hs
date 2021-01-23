{-# LANGUAGE OverloadedStrings #-}

module Day9.Soln where

import qualified Data.Text as T
import Data.Text.Read
import qualified Data.Text.IO as TIO
import Data.Either

import Debug.Trace

-- input

readXMAS :: IO [Int]
readXMAS = do lines <- T.splitOn "\n" <$> TIO.readFile inputFile
              return $ map readInt lines

inputFile :: FilePath
inputFile = "src/Day9/short-input.txt"

readInt :: T.Text -> Int
readInt text = case decimal text of 
                 Left err -> error err 
                 Right (res, "") -> res
