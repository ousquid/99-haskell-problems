import Data.List
import Data.Ord

data GraphTerm a = Graph [a] [(a, a)]
    deriving (Show, Eq)

data Adjacency a = Adj [(a, [a])]
	deriving (Show, Eq)

graphG1 :: GraphTerm Int
graphG1 = Graph [1, 2, 3, 4, 5, 6, 7, 8] [(1, 5), (1, 6), (1, 7), (2, 5), (2, 6), (2, 8), (3, 5), (3, 7), (3, 8), (4, 6), (4, 7), (4, 8)]
graphH1 :: GraphTerm Int
graphH1 = Graph [1, 2, 3, 4, 5, 6, 7, 8] [(1, 2), (1, 4), (1, 5), (6, 2), (6, 5), (6, 7), (8, 4), (8, 5), (8, 7), (3, 2), (3, 4), (3, 7)]
graphSimple :: GraphTerm Int
graphSimple = Graph [1, 2, 3, 4] [(1, 2), (1, 3), (3, 4)]
		   												  
graphToAdj :: (Eq a) => GraphTerm a -> Adjacency a
graphToAdj (Graph [] _)      = Adj []
graphToAdj (Graph (x:xs) ys) = Adj ((x, ys >>= f) : zs)
   where 
      f (a, b) 
         | a == x = [b]
         | b == x = [a]
         | otherwise = []
      Adj zs = graphToAdj (Graph xs ys)

graph86 = Graph ['a','b','c','d','e','f','g','h','i','j'] [('a','b'),('a','e'),('a','f'),('b','c'),('b','g'),('c','d'),('c','h'),('d','e'),('d','i'),('e','j'),('f','h'),('f','i'),('g','i'),('g','j'),('h','j')]
-- [('a',1),('b',2),('c',1),('d',2),('e',3),('f',2),('g',1),('h',3),('i',3),('j',2)]

neighbors :: Eq a => GraphTerm a -> a -> [a]
neighbors graph node = snd $ head $ filter (\x->(fst x) == node) items
    where (Adj items) = graphToAdj graph

degree :: Eq a => GraphTerm a -> a -> Int
degree graph node = length $ neighbors graph node

decdegs :: Eq a => GraphTerm a -> [a]
decdegs graph@(Graph nodes edges) = reverse $ map fst $ sortBy (comparing snd) [(n, d) | n <- nodes, let d = degree graph n]

kColor :: Eq a => GraphTerm a -> [(a, Int)]
kColor graph = kColor' graph [] 1 

canPaint :: Eq a => GraphTerm a -> [(a, Int)] -> a -> Int -> Bool
canPaint graph nc_kp node color = (not isSelfPainted) && (not isNeighborsSame) 
    where (Adj items) = graphToAdj graph
          ns = neighbors graph node
          isSelfPainted = node `elem` [n | (n,c)<-nc_kp]
          isNeighborsSame = color `elem` (concat $ [c | n <- ns, let c = map snd $ filter (\x->(fst x) == n) nc_kp])

kColor' :: Eq a => GraphTerm a -> [(a, Int)] -> Int -> [(a, Int)]
kColor' graph@(Graph nodes edges) now_kp target_color 
    | (length now_kp) >= (length nodes) = now_kp 
    | otherwise = kColor' graph new_kp (target_color+1)
    where coloring [] nc_kp = nc_kp 
          coloring (n:ns) nc_kp = coloring ns new_kp 
            where can_p = canPaint graph nc_kp n target_color 
                  new_kp = nc_kp ++ (if can_p then [(n, target_color)] else [])
          sorted_nodes = decdegs graph
          new_kp = coloring sorted_nodes now_kp