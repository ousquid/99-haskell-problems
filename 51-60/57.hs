data Tree a = Empty | Branch a (Tree a) (Tree a)
              deriving (Show, Eq)
              
leaf x = Branch x Empty Empty

mirror Empty          Empty          = True
mirror (Branch _ a b) (Branch _ x y) = mirror a y && mirror b x
mirror _              _              = False

symmetric Empty          = True
symmetric (Branch _ l r) = mirror l r

add :: Ord a => Tree a -> a -> Tree a
add Empty x = leaf x
add (Branch y left right) x 
 | x >= y = Branch y left (add right x)
 | x < y  = Branch y (add left x) right
 
construct :: Ord a => [a] -> Tree a
construct xs = foldl add Empty xs