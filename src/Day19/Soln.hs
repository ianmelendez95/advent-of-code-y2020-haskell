module Day19.Soln where

import Text.Megaparsec
import qualified Data.Text.IO as TIO

import qualified Data.IntMap.Strict as IntMap

import qualified Day19.Parse as P


type Rules = IntMap.IntMap P.Rule

collectRules :: [P.Decl] -> Rules
collectRules = IntMap.fromList

readSpec :: IO P.FullSpec 
readSpec = P.parseFullSpec "src/Day19/short-input.txt"
     



