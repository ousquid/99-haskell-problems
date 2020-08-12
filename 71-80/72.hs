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

bottom_up :: Tree Char -> String
bottom_up (Node x tree) = (concatMap bottom_up tree) ++ [x]