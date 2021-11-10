module Day19.Soln where

import Text.Megaparsec
import qualified Data.Text.IO as TIO

import qualified Data.IntMap.Strict as IntMap

import Data.List

import qualified Day19.Parse as P


type Rules = IntMap.IntMap P.Rule

test :: IO ()
test = 
  do (decls, msgs) <- readSpec
     let rules = collectRules decls
     print $ resolveCRule rules 0


-- Complex Rule
data CRule = CRExact  !Char
           | CRAltSeq ![[CRule]]

instance Show CRule where 
  show (CRExact c) = "\"" ++ [c] ++ "\""
  show (CRAltSeq seqs) = 
    "( " ++ intercalate " | " (map (unwords . map show) seqs) ++ " )"

resolveCRule :: Rules -> Int -> CRule
resolveCRule rules idx = 
  case IntMap.lookup idx rules of 
    Nothing -> error $ "Rule index doesn't exist: " ++ show idx
    Just (P.RExact c) -> CRExact c
    Just (P.RAltSeq rule_seqs) -> 
      CRAltSeq $ map (map (resolveCRule rules)) rule_seqs


collectRules :: [P.Decl] -> Rules
collectRules = IntMap.fromList

readSpec :: IO P.FullSpec 
readSpec = P.parseFullSpec "src/Day19/short-input.txt"
     



