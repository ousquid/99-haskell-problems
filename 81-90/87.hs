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
graphSimple = Graph [1, 2, 3, 4] [(1, 2), (1, 3), (2, 4)]   -- [1, 2, 4, 3]
graph87 :: GraphTerm Int
graph87 = Graph [1,2,3,4,5,6,7] [(1,2),(2,3),(1,4),(3,4),(5,2),(5,4),(6,7)]
		   												  
graphToAdj :: (Eq a) => GraphTerm a -> Adjacency a
graphToAdj (Graph [] _)      = Adj []
graphToAdj (Graph (x:xs) ys) = Adj ((x, ys >>= f) : zs)
   where 
      f (a, b) 
         | a == x = [b]
         | b == x = [a]
         | otherwise = []
      Adj zs = graphToAdj (Graph xs ys)


getNeighbors :: Eq a => GraphTerm a -> a -> [a]
getNeighbors graph node = snd $ head $ filter (\x->(fst x) == node) items
    where (Adj items) = graphToAdj graph

depthFirst :: Eq a => GraphTerm a -> a -> [a]
depthFirst graph firstNode = depthFirst' [firstNode] graph firstNode
    where depthFirst' visited graph currentNode = foldl updateVisited visited (getNeighbors graph currentNode)
          updateVisited visited node = if node `elem` visited then visited else depthFirst' (visited ++ [node]) graph node
    