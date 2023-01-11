import Data.List
import Data.Char

processRange :: String -> (Integer, Integer)
processRange myRange = ((read a :: Integer), (read (tail b) :: Integer))
  where (a, b) = break (=='-') myRange

checkPairContained :: (Ord a) => (a, a) -> (a, a) -> Integer
checkPairContained (x1, y1) (x2, y2) = if (x1 <= x2 && y1 >= y2) || (x1 >= x2 && y1 <= y2) then 1 else 0

getTotalAssignmentsPairs :: [String] -> Integer
getTotalAssignmentsPairs [] = 0
getTotalAssignmentsPairs (x:xs) = checkPairContained (processRange rangeA) (processRange (tail rangeB)) + getTotalAssignmentsPairs xs
  where (rangeA, rangeB) = break (==',') x

checkPartialPairContained :: (Integer, Integer) -> (Integer, Integer) -> Integer
checkPartialPairContained (x1, y1) (x2, y2) = if null ([x1..y1] `intersect` [x2..y2]) then 0 else 1

getTotalPartialAssignmentsPairs :: [String] -> Integer
getTotalPartialAssignmentsPairs [] = 0
getTotalPartialAssignmentsPairs (x:xs) = checkPartialPairContained (processRange rangeA) (processRange (tail rangeB)) + getTotalPartialAssignmentsPairs xs
  where (rangeA, rangeB) = break (==',') x

main :: IO ()
main = do
  contents <- readFile "section-assignments.txt"
  let linesOfFile = lines contents
  let score = getTotalAssignmentsPairs linesOfFile
  print score
  let partialScore = getTotalPartialAssignmentsPairs linesOfFile
  print partialScore
