import Data.List
import Data.List.Split

data Tree a = Node a [Tree a]
        deriving (Eq, Show)
        
tree1 = Node 'a' []
tree2 = Node 'a' [Node 'b' []]
tree3 = Node 'a' [Node 'b' [Node 'c' []]]
tree4 = Node 'b' [Node 'd' [], Node 'e' []]
tree5 = Node 'a' [
                Node 'f' [Node 'g' []],
                Node 'c' [],
                Node 'b' [Node 'd' [], Node 'e' []]
                ]

ipl :: Tree Char -> Int
ipl tree = ipl' tree 0 

ipl' :: Tree Char -> Int -> Int
ipl' tree@(Node a ts) depth = (sum $ map (\x -> ipl' x (depth+1)) ts) + depth