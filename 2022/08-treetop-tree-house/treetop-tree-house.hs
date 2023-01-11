import Data.List

processFile :: [String] -> [[Int]]
processFile [] = []
processFile (x:xs) = (processLine x):processFile xs

processLine :: String -> [Int]
processLine [] = []
processLine (x:xs) = (read [x] :: Int):processLine xs

-- make a function that takes a list of lists of integers and returns the number of visible trees (a tree is visible if it is the tallest tree in its column and row or if it is on the edge of the forest)
findVisibleTrees :: [[Int]] -> [[Int]] -> [[Int]] -> Int -> Int
findVisibleTrees [] _ _ _ = 0
findVisibleTrees (x:xs) rows cols row = findVisibleTreesRow x rows cols row 0 + findVisibleTrees xs rows cols (row + 1)

findVisibleTreesRow :: [Int] -> [[Int]] -> [[Int]] -> Int -> Int -> Int
findVisibleTreesRow [] _ _ _ _ = 0
findVisibleTreesRow (x:xs) rows cols row col = visible + findVisibleTreesRow xs rows cols row (col + 1)
  where
    rowLeft = take col $ rows !! row
    rowRight = drop (col + 1) $ rows !! row
    colTop = take row $ cols !! col
    colBottom = drop (row + 1) $ cols !! col
    isEdge = row == 0 || row == (length rows - 1) || col == 0 || col == (length cols - 1)
    isVisible = x > maximum rowLeft || x > maximum rowRight || x > maximum colTop || x > maximum colBottom
    visible = if isEdge || isVisible then 1 else 0

highestScenicScore :: [[Int]] -> [[Int]] -> [[Int]] -> Int -> Int -> Int
highestScenicScore [] _ _ maxScore _ = maxScore
highestScenicScore (x:xs) rows cols maxScore row = highestScenicScore xs rows cols currentScore (row + 1)
  where currentScore = highestScenicScoreRow x rows cols maxScore row 0

highestScenicScoreRow :: [Int] -> [[Int]] -> [[Int]] -> Int -> Int -> Int -> Int
highestScenicScoreRow [] _ _ maxScore _ _ = maxScore
highestScenicScoreRow (x:xs) rows cols maxScore row col = highestScenicScoreRow xs rows cols (max maxScore score) row (col + 1)
  where
    rowLeft = reverse (take col $ rows !! row)
    scoreLeft = getTreeScore rowLeft x
    rowRight = drop (col + 1) $ rows !! row
    scoreRight = getTreeScore rowRight x
    colTop = reverse (take row $ cols !! col)
    scoreTop = getTreeScore colTop x
    colBottom = drop (row + 1) $ cols !! col
    scoreBottom = getTreeScore colBottom x
    score = scoreLeft * scoreRight * scoreTop * scoreBottom

getTreeScore :: [Int] -> Int -> Int
getTreeScore [] _ = 0
getTreeScore (x:xs) tree
  | x == tree = 1 + getTreeScore [] tree
  | x < tree = 1 + getTreeScore xs tree
  | otherwise = getTreeScore [] tree

main :: IO ()
main = do
  contents <- readFile "trees.txt"
  let linesOfFile = lines contents
  let rows = processFile linesOfFile
  let cols = transpose rows
  print $ findVisibleTrees rows rows cols 0
  print $ highestScenicScore rows rows cols 0 0