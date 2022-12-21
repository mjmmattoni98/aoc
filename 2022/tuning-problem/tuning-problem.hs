allUnique :: (Eq a) => [a] -> Bool
allUnique [] = True
allUnique (x:xs) = x `notElem` xs && allUnique xs

findStartOfPacket :: String -> String -> Int -> Int
findStartOfPacket [] _ _ = -1
findStartOfPacket (x:xs) packet foundAfter 
  | isPacket = foundAfter
  | otherwise = findStartOfPacket xs packet' (foundAfter + 1)
  where isPacket = allUnique packet
        packet' = tail packet ++ [x]

main :: IO ()
main = do
  contents <- readFile "datastream-buffer.txt"
  let line = head $ lines contents
  let packet = take 14 line
  let startOfPacket = findStartOfPacket (drop 14 line) packet 14
  print startOfPacket