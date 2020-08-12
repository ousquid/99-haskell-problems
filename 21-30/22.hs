range :: Int -> Int -> [Int]

range a b
  | a == b = [a]
  | otherwise = [a] ++ range (a+1) b