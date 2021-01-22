module Day1.Soln where

readMasses :: IO [Int]
readMasses = map (read :: String -> Int) . lines <$> readFile "src/Day1/full-input.txt"

massToFuel :: Int -> Int
massToFuel = (+(-2)) . (`div` 3) 

main :: IO ()
main = do ls <- map (read :: String -> Int) . lines <$> readFile "src/Day1/full-input.txt"
          print $ sum (fmap massToFuel ls)
