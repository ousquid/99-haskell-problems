-- combinations 3 "abcdef"
-- ["abc","abd","abe",...]
import Data.List

combinations :: Ord a => Int -> [a] -> [[a]]
combinations 1 xs = [[x] | x <- xs]
combinations n xs = nub (map sort permu)
   where permu = [[x] ++ c | x <- xs, c <- (combinations (n-1) (delete x xs))]