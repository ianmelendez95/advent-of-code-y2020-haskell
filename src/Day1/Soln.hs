module Day1.Soln where

readMasses :: IO [Int]
readMasses = map (read :: String -> Int) . lines <$> readFile "src/Day1/full-input.txt"

massToFuel :: Int -> Int
massToFuel mass
  | mass <= 0 = 0
  | otherwise = let fuel = (mass `div` 3) - 2 in fuel + massToFuel fuel 

main :: IO ()
main = do ls <- map (read :: String -> Int) . lines <$> readFile "src/Day1/full-input.txt"
          print $ sum (fmap massToFuel ls)
