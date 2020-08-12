import Data.List

data GraphTerm = Graph [Int] [(Int, Int)]
    deriving (Eq, Show)

graphG1 = Graph [1,2,3,4,5,6,7,8] [(1,5),(1,6),(1,7),(2,5),(2,6),(2,8),(3,5),(3,7),(3,8),(4,6),(4,7),(4,8)]
graphH1 = Graph [1,2,3,4,5,6,7,8] [(1,2),(1,4),(1,5),(6,2),(6,5),(6,7),(8,4),(8,5),(8,7),(3,2),(3,4),(3,7)]

graphA = Graph [1,2,3] [(1,2), (1,3)]
graphB = Graph [1,2,3] [(1,2), (2,3)]

iso :: GraphTerm -> GraphTerm -> Bool
iso (Graph an ae) g@(Graph bn be) = or [(sort $ map sortTuple ae) == (sort $ map sortTuple ce) | ce <- permedEdges g]
    where sortTuple (a, b) = if a < b then (a, b) else (b, a)

permedEdges :: GraphTerm -> [[(Int, Int)]]
permedEdges graph@(Graph n e) = [edges | rule <- permedNodes, let Graph _ edges = f graph rule]
    where permedNodes = permutations n
    
f :: GraphTerm -> [Int] -> GraphTerm
f (Graph n e) rule = Graph n (foldl (\acc x -> acc ++ [fEdge x]) [] e)
    where fEdge (a, b) = (rule !! (a-1), rule !! (b-1))
    
    
 