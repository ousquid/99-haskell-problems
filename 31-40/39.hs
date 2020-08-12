primesR :: Int -> Int -> [Int]
primesR x y =filter isPrime [x..y]

isPrime :: Int -> Bool
isPrime x = foldl (\acc y -> (x `mod` y) /= 0 && acc) True [2..ceiling (sqrt (fromIntegral x))]