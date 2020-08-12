goldbach :: Int -> (Int, Int)
goldbach x = (goldbach', x - goldbach')
  where goldbach' = head $ dropWhile (\y -> not $ isPrime y && isPrime (x - y)) [2..x-1]

isPrime :: Int -> Bool
isPrime x = foldl (\acc y -> (x `mod` y) /= 0 && acc) True [2..ceiling (sqrt (fromIntegral x))]