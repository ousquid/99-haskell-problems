data Tree a = Empty | Branch a (Tree a) (Tree a)
              deriving (Show, Eq)
              
leaf x = Branch x Empty Empty

cbalTree :: Int -> [Tree String]
cbalTree 0 = [Empty]
cbalTree n = [Branch "x" left right | i <- [half .. prev_n-half], 
                                      left <- cbalTree (prev_n-i), 
                                      right <- cbalTree i]
    where prev_n = n - 1
          half = prev_n `div` 2

symmetric :: Eq a => Tree a -> Bool
symmetric x = (x == flip_xtree x)

flip_xtree :: Tree a -> Tree a
flip_xtree Empty = Empty
flip_xtree (Branch a left right) = Branch a (flip_xtree right) (flip_xtree left)

symCbalTrees :: Int -> [Tree String]
symCbalTrees n = filter symmetric (cbalTree n)
