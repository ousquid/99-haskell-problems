import Data.List
import Data.Ord

data GraphTerm a = Graph [a] [(a, a)]
    deriving (Show, Eq)

data Adjacency a = Adj [(a, [a])]
	deriving (Show, Eq)

graphSimple :: [(Int,Int)]
graphSimple = [(1,2),(2,3),(3,4)]
graphTest :: [(Int,Int)]
graphTest = [(1,6),(2,6),(3,6),(4,6),(5,6),(5,7),(5,8),(8,9),(5,10),(10,11),(11,12),(11,13),(13,14)]
-- [6,7,8,9,3,4,10,11,5,12,2,13,14,1]

graphToAdj :: (Eq a) => GraphTerm a -> Adjacency a
graphToAdj (Graph [] _)      = Adj []
graphToAdj (Graph (x:xs) ys) = Adj ((x, ys >>= f) : zs)
    where 
      f (a, b) 
         | a == x = [b]
         | b == x = [a]
         | otherwise = []
      Adj zs = graphToAdj (Graph xs ys)


vonKoch :: [(Int,Int)] -> [[Int]]
vonKoch edges = concat [vonKoch' (Adj items) partialNodeDict | partialNodeDict <- partialNodeDictWithN]
    where vertices = nub $ concatMap (\x -> [fst x, snd x]) edges
          (Adj items) = graphToAdj (Graph vertices edges)
          partialNodeDictWithN = [[(idx, n)] | idx <- [1..n]]
          n = length items
          
-- partialNodeDict : [node, assignNumber]
vonKoch' :: Adjacency Int -> [(Int,Int)] -> [[Int]]
vonKoch' entireGraph@(Adj items) partialNodeDict
    | diff == 0 = [map snd $ sort partialNodeDict]
    | otherwise = concatMap (vonKoch' entireGraph) [(neighborVertex, assignNumber):partialNodeDict | (partialNode, neighborVertex) <- candidateEdges, assignNumber <- getAssignNumber partialNode partialNodeDict diff]
        where diff = (length items) - (length partialNodeDict)
              partialNodeList = map fst partialNodeDict
              candidateEdges = [(partialNode, neighborVertex) | partialNode <- partialNodeList, neighborVertex <- snd (head $ filter (\x->(fst x) == partialNode) items), neighborVertex `notElem` partialNodeList]
              getAssignNumber partialNode partialNodeDict diff = [assignNumber | sign <- [-1, 1], let assignNumber = partialNodeAssignNumber + sign * diff, assignNumber > 0, assignNumber <= (length items), assignNumber `notElem` (map snd partialNodeDict)]
                    where partialNodeAssignNumber = snd $ head $ filter (\x -> (fst x) == partialNode) partialNodeDict
