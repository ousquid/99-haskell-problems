import Data.List

data GraphTerm a = Graph [a] [(a,a)]
        deriving (Eq, Show)
data AdjacencyList a = Adj [(a, [a])]
        deriving (Eq, Show)

k4 = Graph ['a', 'b', 'c', 'd'] [('a', 'b'), ('b', 'c'), ('c', 'd'), ('d', 'a'), ('a', 'c'), ('b', 'd')]
k4_term1 = Graph ['a','b','c','d','e','f','g','h'] [('a','b'),('a','d'),('b','c'),('b','e'),('c','e'),('d','e'),('d','f'),('d','g'),('e','h'),('f','g'),('g','h')]
sampe_term = Graph ['a','b','c','d'] [('a','b'),('b','c'),('c','d'),('d','a'),('c','a')]

graphToAdj :: GraphTerm Char -> AdjacencyList Char
graphToAdj (Graph keys edges) = Adj [(key, concat (neighbors key edges)) | key<-keys]
                    where neighbors k es = map (edgepair k) es
                          edgepair k e 
                                | fst e == k = [snd e]
                                | snd e == k = [fst e]
                                | otherwise = ""

adjToGraph :: AdjacencyList Char -> GraphTerm Char
adjToGraph (Adj adjs) = Graph vertices edges
                            where vertices = map fst adjs
                                  edges = [(k,n) | (k, neighbors)<- adjs, n<-neighbors, k<n]
                                  sedges = sort (nub edges)

-- ((a b c d e f g h) ((a b) (a d) (b c) (b e) (c e) (d e) (d f) (d g) (e h) (f g) (g h)))

-- length $ spanningTree k4

spanningTree :: Eq a => GraphTerm a -> [GraphTerm a] 
spanningTree graph@(Graph vertices edges) = [(Graph vertices es) | es<-treeComb graph, is_tree es]

comb :: Eq a => Int -> [(a,a)] -> [[(a,a)]]
comb 0 xs = [[]]
comb _ [] = []
comb n (x:xs) = [x:y | y <- comb (n-1) xs] ++ comb n xs

treeComb :: Eq a => GraphTerm a -> [[(a,a)]]
treeComb (Graph vertices edges) = comb vnum edges
    where vnum = (length vertices) - 1

is_tree :: Eq a => [(a,a)] -> Bool
is_tree (x:xs) = is_tree' xs [fst x, snd x]

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

-- is_connected ::  GraphTerm a -> Bool
-- is_connected graph = ((length . spanningTree) graph)==1