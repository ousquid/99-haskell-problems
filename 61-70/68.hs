data Tree a = Empty | Branch a (Tree a) (Tree a)
              deriving (Show, Eq)
leaf x = Branch x Empty Empty
              
tree65 = Branch 'n'
                (Branch 'k'
                        (Branch 'c'
                                (Branch 'a' Empty Empty)
                                (Branch 'e'
                                        (Branch 'd' Empty Empty)
                                        (Branch 'g' Empty Empty)
                                )
                        )
                        (Branch 'm' Empty Empty)
                )
                (Branch 'u'
                        (Branch 'p'
                                Empty
                                (Branch 'q' Empty Empty)
                        )
                        Empty
                )
                
tree67 = Branch 'a' (Branch 'b' (Branch 'd' Empty Empty) (Branch 'e' Empty Empty)) (Branch 'c' Empty (Branch 'f' (Branch 'g' Empty Empty) Empty))

treeToPreorder :: Tree Char -> String
treeToPreorder Empty = ""
treeToPreorder (Branch a l r) = [a] ++ treeToPreorder l ++ treeToPreorder r

treeToInorder :: Tree Char -> String
treeToInorder Empty = ""
treeToInorder (Branch a l r) = treeToInorder l ++ [a] ++ treeToInorder r

preorderToTree :: String -> [Tree Char]
preorderToTree [] = [Empty]
preorderToTree (x:xs) = [Branch x ltree rtree | (l,r) <- [splitAt i xs | i <- [0..(length xs)]],
                                                ltree <- preorderToTree l,
                                                rtree <- preorderToTree r]
                                                
preInTree :: String -> String -> Tree Char
preInTree po io = head $ filter (\x -> treeToInorder x == io) trees
   where trees = preorderToTree po

-- inorderToTree :: String -> [Tree Char]
-- inorderToTree [] = [Empty]
-- inorderToTree xs = [Branch x ltree rtree | (l,x:r) <- [splitAt i xs | i <- [0..(length xs - 1)]],
--                                                 ltree <- inorderToTree l,
--                                                 rtree <- inorderToTree r]
