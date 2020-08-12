import Data.List
-- paths :: Int -> Int -> [(Int, Int)]
-- paths from to graph = filter (\x -> (head x) == from && (last x) == to) (allPath graph)

-- allPath :: [(Int, Int)] -> [[Int]]
-- allPath graph = permutations graph

-- start: 2
-- end: 3
-- [(1,2), (2,3)]
-- [(2,1), (2,3)]

-- [(1,3), (2,3), (3,4)] x
-- [(1,3), (2,3), (2,4)] o



-- 問題文に騙されて無向グラフに対しての解答を作った
-- paths :: Int -> Int -> [(Int, Int)] -> [[Int]]
-- paths from to graph = map (nub . foldl (\x y -> x ++ [fst y, snd y]) [from]) (paths' [from] from to graph)

-- paths' :: [Int] -> Int -> Int -> [(Int, Int)] -> [[(Int, Int)]]
-- paths' passedList from to graph 
--  | from == to = [[]]
--  | otherwise = [[a]++b | a <- edges from graph, not ((next a) `elem` passedList), b <- paths' (passedList ++ [(next a)]) (next a) to graph]
--     where edges node graph = filter (\x -> fst x == node || snd x == node) graph
--           next a = if fst a == from then snd a else fst a

    
-- 1 -> 2 -> 3

-- start : 1 
-- goal : 3

-- (from 2) passedList: [1,2]
-- a: 2->1, 2->3
-- b: 2->1 paths 


paths :: Int -> Int -> [(Int, Int)] -> [[Int]]
paths from to graph = map (nub . foldl (\x y -> x ++ [fst y, snd y]) []) (paths' [from] from to graph)

paths' :: [Int] -> Int -> Int -> [(Int, Int)] -> [[(Int, Int)]]
paths' passedList from to graph 
 | from == to = [[]]
 | otherwise = [[a]++b | a <- edges from graph, not ((snd a) `elem` passedList), b <- paths' (passedList ++ [(snd a)]) (snd a) to graph]
    where edges node graph = filter (\x -> fst x == node) graph
