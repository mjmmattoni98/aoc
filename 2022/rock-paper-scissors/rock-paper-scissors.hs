getLineScore :: String -> String -> Integer
getLineScore rival player = case (rival, player) of
  ("A", "X") -> 3
  ("A", "Y") -> 6
  ("A", "Z") -> 0
  ("B", "X") -> 0
  ("B", "Y") -> 3
  ("B", "Z") -> 6
  ("C", "X") -> 6
  ("C", "Y") -> 0
  ("C", "Z") -> 3

getDecision :: String -> String -> String
getDecision rival player = case (rival, player) of
  ("A", "X") -> "Z"
  ("A", "Y") -> "X"
  ("A", "Z") -> "Y"
  ("B", "X") -> "X"
  ("B", "Y") -> "Y"
  ("B", "Z") -> "Z"
  ("C", "X") -> "Y"
  ("C", "Y") -> "Z"
  ("C", "Z") -> "X"

getResultScore :: String -> Integer
getResultScore result = case result of
  "X" -> 0
  "Y" -> 3
  "Z" -> 6

getDecisionScore :: String -> Integer
getDecisionScore decision = case decision of
  "X" -> 1
  "Y" -> 2
  "Z" -> 3

getTotalScore :: [String] -> Integer
getTotalScore [] = 0
getTotalScore (x:xs) = getLineScore rival player + getDecisionScore player + getTotalScore xs
  where (rival:player:_) = words x

getTotalScore' :: [String] -> Integer
getTotalScore' [] = 0
getTotalScore' (x:xs) = getResultScore result + getDecisionScore decision + getTotalScore' xs
  where (rival:player:_) = words x
        decision = getDecision rival player
        result = player

main :: IO ()
main = do
  contents <- readFile "matrix-decisions.txt"
  let linesOfFile = lines contents
  let score = getTotalScore linesOfFile
  print score
  let score' = getTotalScore' linesOfFile
  print score'
