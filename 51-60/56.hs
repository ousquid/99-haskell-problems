data Tree a = Empty | Branch a (Tree a) (Tree a)
              deriving (Show, Eq)
leaf x = Branch x Empty Empty

symmetric :: Eq a => Tree a -> Bool
symmetric x = (x == flip_xtree x)

flip_xtree :: Tree a -> Tree a
flip_xtree Empty = Empty
flip_xtree (Branch a left right) = Branch a (flip_xtree right) (flip_xtree left)  