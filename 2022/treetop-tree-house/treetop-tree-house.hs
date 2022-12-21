processFile :: [String] -> [[Int]]
processFile [] = []
processFile (x:xs) = (processLine x):processFile xs

processLine :: String -> [Int]
processLine [] = []
processLine (x:xs) = (read [x] :: Int):processLine xs

-- make a function that takes a list of lists of integers and returns the number of visible trees (a tree is visible if it is the tallest tree in its column and row or if it is on the edge of the forest)



main :: IO ()
main = do
  contents <- readFile "trees.txt"
  let linesOfFile = lines contents
