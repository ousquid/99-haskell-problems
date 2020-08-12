import Data.List

regular :: Int -> Int -> [[(Int, Int)]]
regular n k
    | n * k `mod` 2 == 0 = regular' regularEdges []
    | otherwise = [[]]
    where es = edges n
          edgePatterns = combinations (n*k `div` 2) es
          regularEdges = filter (isKRegular k) edgePatterns
          
regular' :: [[(Int, Int)]] -> [[(Int, Int)]] -> [[(Int, Int)]]
regular' [] checked = checked
regular' rest checked = regular' (rest \\ (map sort $ isomorphisms g)) (g:checked)
    where g = head rest

combinations :: Int -> [a] -> [[a]]
combinations n = filter ((== n) . length) . subsequences

edges :: Int -> [(Int, Int)]
edges n = [(a,b) | a <- [1..n], b <- [1..n], a < b]

isKRegular :: Int -> [(Int, Int)] -> Bool
isKRegular k = all (== k) . map length . group . sort . foldl (\xs (a,b) -> a:b:xs) []

isomorphisms :: [(Int, Int)] -> [[(Int, Int)]]
isomorphisms es = [ f es rule | rule <- permutations [1..n]]
    where n = length $ nub $ foldl (\xs (a,b) -> a:b:xs) [] es

f :: [(Int, Int)] -> [Int] -> [(Int, Int)]
f e rule = foldl (\acc x -> acc ++ [fEdge x]) [] e
    where fEdge (a, b) = if newA < newB then (newA, newB) else (newB, newA)
            where newA = rule !! (a-1)
                  newB = rule !! (b-1)
