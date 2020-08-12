import Data.List

queens :: Int -> [[Int]]
queens len = queens' len []

queens' :: Int -> [Int] -> [[Int]]
queens' len selected
 | length selected == len = [selected]
 | otherwise = concat [queens' len (selected++[x]) | x <- [1..len], isValid x]
    where isValid x = not $ x `elem` (nub $ concat $ map (\(f,s)-> [f+s,f,f-s])$ zip (reverse selected) [1..])
