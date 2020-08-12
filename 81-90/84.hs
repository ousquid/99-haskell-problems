import Data.List

data LabelledGraphTerm a = LabelledGraph [a] [(a,a,Int)]
        deriving (Eq, Show)
data LabelledAdjacencyList a = LabelledAdj [(a, [(a,Int)])]
        deriving (Eq, Show)
data LabelledHumanFrendly = LabelledPretty [[Char]]
        deriving (Eq, Show)

k4 = LabelledGraph [1,2,3,4,5] [(1,2,12),(1,3,34),(1,5,78),(2,4,55),(2,5,32),(3,4,61),(3,5,44),(4,5,93)]

is_tree' :: Eq a => [(a,a)] -> [a] -> Bool
is_tree' [] _ =  True
is_tree' remains visited 
    | cnum == 0 = False 
    | otherwise = is_tree' next_remains next_visited 
    where neighbors vs (f,s) = length (intersect vs [f, s]) 
          cands = filter (\x->(neighbors visited x)==1) remains
          cnum = length cands
          next_remains = filter ((/=) (head cands)) remains
          next_visited = nub (visited ++ [fst (head cands), snd (head cands)])
          
          
prim :: Eq a => LabelledGraphTerm a -> [(a, a, Int)]
prim (LabelledGraph nodes@(x:xs) edges) = prim' (LabelledGraph [x] []) xs edges 

-- PartialTree -> remainingNodes -> RemainingEdgeList -> Tree
prim' :: Eq a => LabelledGraphTerm a -> [a] -> [(a, a, Int)] -> [(a, a, Int)]
prim' (LabelledGraph nodes edges) [] _ = edges
prim' (LabelledGraph partialTreeNodes partialTreeEdges) remainingNodes remainingEdges = prim' newPartialTree newRemainingNodes newRemainingEdges 
    where newNode = if (fst3 selectedEdge) `elem` partialTreeNodes then (snd3 selectedEdge) else (fst3 selectedEdge)
          selectedEdge = foldr1 (\a@(_,_,x) b@(_,_,y) -> if x < y then a else b) cands
          neighbors vs (f,s,_) = length (intersect vs [f, s]) 
          cands = filter (\x->(neighbors partialTreeNodes x)==1) remainingEdges
          newPartialTree = LabelledGraph (newNode:partialTreeNodes) (selectedEdge:partialTreeEdges)
          newRemainingNodes = filter ((/=) newNode) remainingNodes
          newRemainingEdges = filter ((/=) selectedEdge) remainingEdges
          
          
fst3 (a, _, _) = a
snd3 (_, b, _) = b
trd3 (_, _, c) = c