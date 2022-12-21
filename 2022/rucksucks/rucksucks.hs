import Data.List 

-- Function that takes a list of tuples and a character, and returns the value of the tuple that has the character as its first element.
getPriority :: [(Char, Integer)] -> Char -> Integer
getPriority [] _ = 0
getPriority ((x,y):xs) c
    | x == c = y
    | otherwise = getPriority xs c

getRepeated :: String -> String -> Char
-- getRepeated [] _ = ' '
-- getRepeated (x:xs) string' = if x `elem` string' then x else getRepeated xs string'
-- getRepeated string string' = head [x | x <- string, x `elem` string']
-- getRepeated stringA stringB = head $ filter (`elem` stringB) stringA
getRepeated stringA stringB = head (stringA `intersect` stringB)

getRepeatedBatch :: String -> String -> String -> Char
getRepeatedBatch stringA stringB stringC = head (stringA `intersect` (stringB `intersect` stringC))

getPrioritySum :: [String] -> Integer
getPrioritySum [] = 0
getPrioritySum (x:xs) = getPriority priorities (getRepeated stringA stringB) + getPrioritySum xs
  where (stringA, stringB) = splitAt (length x `div` 2) x
        priorities = zip (['a'..'z'] ++ ['A'..'Z']) [1..]

getPriorityBatchSum :: [String] -> Integer
getPriorityBatchSum [] = 0
getPriorityBatchSum (x:y:z:xs) = getPriority priorities (getRepeatedBatch x y z) + getPriorityBatchSum xs
  where priorities = zip (['a'..'z'] ++ ['A'..'Z']) [1..]

main :: IO ()
main = do
  contents <- readFile "rucksucks.txt"
  let linesOfFile = lines contents
  let prioritySum = getPrioritySum linesOfFile
  print prioritySum
  let priorityBatchSum = getPriorityBatchSum linesOfFile
  print priorityBatchSum
