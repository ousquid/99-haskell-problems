repli :: [a] -> Int -> [a]
repli x n = concatMap (replicate n) x