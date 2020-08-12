-- λ> goldbachList 9 20
-- [(3,7),(5,7),(3,11),(3,13),(5,13),(3,17)]
-- λ> goldbachList' 4 2000 50
--[(73,919),(61,1321),(67,1789),(61,1867)]
import Data.Maybe
import Data.List

goldbachList :: Int -> Int -> [(Int, Int)]
goldbachList x y = goldbachList' x y 1

goldbachList' :: Int -> Int -> Int -> [(Int, Int)]
goldbachList' x y z = filter (\t-> fst t > z && snd t > z) [fromJust g | a <- [x..y], even a, let g = goldbach a 1, isJust g]

goldbach :: Int -> Int -> Maybe (Int, Int)
goldbach x z = case goldbach' of
                Nothing -> Nothing
                Just y -> Just (y, x - y)
  where goldbach' = find (\y -> isPrime y && isPrime (x - y)) [z+1..x-z]

isPrime :: Int -> Bool
isPrime x = foldl (\acc y -> (x `mod` y) /= 0 && acc) True [2..ceiling (sqrt (fromIntegral x))]