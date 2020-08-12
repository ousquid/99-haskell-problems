import System.Random
import GHC.Base

diff_select :: Int -> Int -> IO [Int]
diff_select n m = diff_select' n [1..m]

diff_select' :: Int -> [Int] -> IO [Int]
diff_select' n xs 
 | n==0  = return []
 | otherwise = do
    gen <- getStdGen
    let index = fst $ randomR (1, length xs) gen
    let remain = (take (index-1) xs) ++ (drop index xs)
    result_tail <- diff_select' (n-1) remain
    return ([xs!!(index-1)] ++ result_tail)
