dropEvery :: [a] -> Int -> [a]
dropEvery xs n = [x | (x, y) <- zip xs [1..], y `mod` n /= 0]