import Data.List

-- Implement a replaceAt function that takes a list, an index, and a new value, and returns a new list with the element at the given index replaced with the new value. For example, replaceAt [1,2,3,4,5] 2 10 should return [1,2,10,4,5].
replaceAt :: [a] -> Int -> a -> [a]
replaceAt xs i x = take i xs ++ [x] ++ drop (i+1) xs

trim :: [String] -> [String]
trim [] = []
trim (x:xs) = (dropWhile (== ' ') x):trim xs

-- Read a file and return a tuple of two lists of strings, where the first list contains the lines of the file until it reaches an empty line, and the second list contains the rest of the lines of the file.
processFile :: [String] -> ([String], [String])
processFile [] = ([], [])
processFile (x:xs)
  | x == "" = ([], xs)
  | otherwise = (x:ys, zs)
  where (ys, zs) = processFile xs

processStacks :: [String] -> Int -> [String]
processStacks [] _ = []
processStacks (x:xs) lengthStack = [x !! y | y <- [1,5..lengthStack]]:processStacks xs lengthStack

processMoves :: [String] -> [[Int]]
processMoves [] = []
processMoves (x:xs) = [read move :: Int, read from :: Int, read to :: Int]:processMoves xs
  where (_:move:_:from:_:to:_) = words x

makeMoves :: [[Int]] -> [String] -> [String]
makeMoves [] stacks = stacks
makeMoves (x:xs) stacks = makeMoves xs (moveStacks x stacks)

moveStacks :: [Int] -> [String] -> [String]
moveStacks (move:from:to:_) stacks = stacks'
  where
    (from', to') = (from - 1, to - 1)
    elementsToMove = take move (stacks !! from')
    stackRemovedItems = drop move (stacks !! from')
    stackAddedItems = elementsToMove ++ (stacks !! to')
    stacks' = replaceAt (replaceAt stacks from' stackRemovedItems) to' stackAddedItems

main :: IO ()
main = do
  contents <- readFile "stacks.txt"
  let linesOfFile = lines contents
  let (firstLines, moves) = processFile linesOfFile
  let initialStacks = init firstLines
  let numberOfStacks = length $ words $ last firstLines
  let numberOfChar = numberOfStacks * 4 - 1
  let stacks = trim $ transpose $ processStacks initialStacks numberOfChar
  let actions = processMoves moves
  print stacks
  let finalStacks = makeMoves actions stacks
  print $ head $ transpose finalStacks
