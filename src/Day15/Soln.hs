module Day15.Soln where 

inputFile :: FilePath 
inputFile = "src/Day15/short-input.txt"

readInput :: IO [String]
readInput = readCSV <$> readFile inputFile

readCSV :: String -> [String]
readCSV [] = []
readCSV input = 
  let (item, rest) = span (/= ',') input
   in item : readCSV (dropWhile (== ',') rest)
