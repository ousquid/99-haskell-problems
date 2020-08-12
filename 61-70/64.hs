data Tree a = Empty | Branch a (Tree a) (Tree a)
              deriving (Show, Eq)
leaf x = Branch x Empty Empty

tree64 = Branch 'n'
                (Branch 'k'
                        (Branch 'c'
                                (Branch 'a' Empty Empty)
                                (Branch 'h'
                                        (Branch 'g'
                                                (Branch 'e' Empty Empty)
                                                Empty
                                        )
                                        Empty
                                )
                        )
                        (Branch 'm' Empty Empty)
                )
                (Branch 'u'
                        (Branch 'p'
                                Empty
                                (Branch 's'
                                        (Branch 'q' Empty Empty)
                                        Empty
                                )
                        )
                        Empty
                )

layout :: Tree a -> Tree (a, (Int, Int))
layout t = snd $ layout' t 1 1

layout' :: Tree a -> Int -> Int -> (Int, Tree (a, (Int, Int)))
layout' Empty position _ = (position-1, Empty)
layout' (Branch x l r) position depth = (lastOfRight, Branch (x, (lastOfLeft+1, depth)) newLeft newRight)
        where (lastOfLeft, newLeft) = layout' l position (depth+1)
              (lastOfRight, newRight) = layout' r (lastOfLeft+1+1) (depth+1)
              
