import Data.List

primeFactorsMult :: Int -> [(Int, Int)]
primeFactorsMult = map (\xs -> (head xs, length xs)) . group . primeFactors


primeFactors :: Int -> [Int]
primeFactors x 
 | x == 1    = []
 | otherwise = factor : primeFactors (x `div` factor)
 where
  factor = head $ filter (\y -> x `mod` y == 0) primeList
  primeList = [2..x] -- 2:3:[z + i | z <- [6,12..x], i <- [-1,1]]
