import qualified Data.Map as Map
import Data.List

maxSize = 100000
spaceNeeded = 30000000

processFile :: [String] -> [String] -> Map.Map String Int -> Map.Map String Int
processFile [] _ map = map
processFile (x:xs) route map 
  | isCd = if command !! 2 == ".." then processFile xs (init route) map else processFile xs (route ++ [command !! 2]) map
  | isLs = processFile xs route map
  | isDir = processFile xs route map
  | otherwise = processFile xs route (addSizeToDirs route (read (head command)) map)
  where command = words x
        isCd = head command == "$" && command !! 1 == "cd"
        isLs = head command == "$" && command !! 1 == "ls"
        isDir = head command == "dir"

addSizeToDirs :: [String] -> Int -> Map.Map String Int -> Map.Map String Int
addSizeToDirs [] _ map = map
addSizeToDirs route size map = Map.insertWith (+) (concat route) size (addSizeToDirs (init route) size map)

getSumSizes :: [Int] -> Int
getSumSizes [] = 0
getSumSizes (x:xs) 
  | x > maxSize = getSumSizes xs
  | otherwise = x + getSumSizes xs

sizeDirToDelete :: [Int] -> Int -> Int -> Int
sizeDirToDelete [] _ space = space
sizeDirToDelete (x:xs) spaceLeft space 
  | x + spaceLeft >= spaceNeeded = sizeDirToDelete [] spaceLeft x
  | otherwise = sizeDirToDelete xs spaceLeft space

main :: IO()
main = do
  contents <- readFile "commands.txt"
  let linesOfFile = lines contents
  let sizes = Map.elems (processFile (tail linesOfFile) [""] Map.empty)
  let sumSizes = getSumSizes sizes
  print sumSizes
  let spaceLeft = 70000000 - maximum sizes
  print $ sizeDirToDelete (sort sizes) spaceLeft 0
  