slice :: [a] -> Int -> Int -> [a]
slice xs n m = drop (n - 1) $ take m xs