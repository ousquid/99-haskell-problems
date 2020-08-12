import Data.List

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
-- (a (f g) c (b d e))

display_lisp :: Tree Char -> String
display_lisp (Node x []) = [x]
display_lisp (Node x children) = "(" ++ [x] ++ " " ++ (concat $ intersperse " " (map display_lisp children)) ++ ")"

tree_ltl :: String -> Tree Char
tree_ltl xs = head $ fst (tree_ltl' $ filter (/=' ') xs)
--tree_ltl xs = snd (tree_ltl' $ filter (/=' ') xs)
    where tree_ltl' (x:xs)
            | x == '(' = ([Node (head xs) trees0] ++ trees1, rest1)
            | x == ')' = ([], xs)
            | otherwise = ([Node x []] ++ trees2, rest2)
                where (trees0, rest0) = tree_ltl' (tail xs)
                      (trees1, rest1) = tree_ltl' rest0
                      (trees2, rest2) = tree_ltl' xs












