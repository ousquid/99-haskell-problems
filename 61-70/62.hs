data Tree a = Empty | Branch a (Tree a) (Tree a)
              deriving (Show, Eq)
leaf x = Branch x Empty Empty

tree4 = Branch 1 (Branch 2 Empty (Branch 4 Empty Empty))
                 (Branch 2 Empty Empty)

internals :: Tree a -> [a]
internals Empty = []
internals (Branch _ Empty Empty) = []
internals (Branch n l r) = (internals l)++(internals r)++[n]

atLevel :: Tree a -> Int -> [a]
atLevel Empty _ = []
atLevel (Branch n _ _) 1 = [n]
atLevel (Branch _ l r) level = (atLevel l (level - 1)) ++ (atLevel r (level - 1))