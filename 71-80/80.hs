import Data.List

data GraphTerm a = Graph [a] [(a,a)]
        deriving (Eq, Show)
data AdjacencyList a = Adj [(a, [a])]
        deriving (Eq, Show)
data HumanFrendly = Pretty [[Char]]
        deriving (Eq, Show)

data DirGraphTerm a = DirGraph [a] [(a,a)]
        deriving (Eq, Show)
data DirAdjacencyList a = DirAdj [(a, [a])]
        deriving (Eq, Show)
data DirHumanFrendly = DirPretty [[Char]]
        deriving (Eq, Show)

data LabelledGraphTerm a = LabelledGraph [a] [(a,a,Int)]
        deriving (Eq, Show)
data LabelledAdjacencyList a = LabelledAdj [(a, [(a,Int)])]
        deriving (Eq, Show)
data LabelledHumanFrendly = LabelledPretty [[Char]]
        deriving (Eq, Show)


term1 = Graph ['b','c','d','f','g','h','k'] [('b','c'),('b','f'),('c','f'),('f','k'),('g','h')]
adj1 = Adj [('b', "cf"), ('c', "bf"), ('d', ""), ('f', "bck"), ('g', "h"), ('h', "g"), ('k', "f")] 

-- graphToAdj Graph ['b','c','d','f','g','h','k'] [('b','c'),('b','f'),('c','f'),('f','k'),('g','h')]
-- Adj [('b', "cf"), ('c', "bf"), ('d', ""), ('f', "bck"), ('g', "h"), ('h', "g"), ('k', "f")]

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