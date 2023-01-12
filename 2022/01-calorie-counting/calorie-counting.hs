import Data.List

findMaxCalories :: [String] -> Integer -> Integer -> Integer
findMaxCalories [] maxValue _ = maxValue
findMaxCalories (x:xs) maxValue currentCalories
  | x == "" = findMaxCalories xs (max maxValue currentCalories) 0
  | otherwise = findMaxCalories xs maxValue (currentCalories + (read x :: Integer))

findTopThreeMaxCalories :: [String] -> [Integer] -> Integer -> Integer
findTopThreeMaxCalories [] (calories1:calories2:calories3:xs) _ = calories1 + calories2 + calories3
findTopThreeMaxCalories (x:xs) calories currentCalories
  | x == "" = findTopThreeMaxCalories xs (sortOn (*(-1)) (currentCalories:calories)) 0
  | otherwise = findTopThreeMaxCalories xs calories (currentCalories + (read x :: Integer))

main :: IO ()
main = do
  contents <- readFile "elf-calories.txt"
  let linesOfFile = lines contents
  let maxCalories = findMaxCalories linesOfFile 0 0
  print maxCalories
  let topThreeMaxCalories = findTopThreeMaxCalories linesOfFile [] 0
  print topThreeMaxCalories
