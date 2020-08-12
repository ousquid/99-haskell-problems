data Tree a = Empty | Branch a (Tree a) (Tree a)
              deriving (Show, Eq)
leaf x = Branch x Empty Empty

tree4 = Branch 1 (Branch 2 Empty (Branch 4 Empty Empty))
                 (Branch 2 Empty Empty)

completeBinaryTree :: Int -> Tree Char
completeBinaryTree n = completeBinaryTree' n 1

completeBinaryTree' :: Int -> Int -> Tree Char
completeBinaryTree' n i 
   | n < i     = Empty
   | otherwise = Branch 'x' (completeBinaryTree' n (i*2)) (completeBinaryTree' n (i*2+1))

isCompleteBinaryTree :: Tree Char -> Bool 
isCompleteBinaryTree tree = completeBinaryTree count == tree
                        where count = countNode tree 

countNode :: Tree a -> Int
countNode Empty = 0
countNode (Branch n l r) = (countNode l) + (countNode r) + 1
