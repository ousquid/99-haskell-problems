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

depth :: Tree a -> Int
depth Empty = 0
depth (Branch _ l r) = 1 + max (depth l) (depth r)

rootPos :: Tree a -> Int
rootPos t = (+1) $ sum $ drop (d - leftChildNum) $ take (d - 1) [2^n | n <- [0,1..]]
  where d = depth t
        leftChildNum = leftChildNum' t
        leftChildNum' Empty = 0
        leftChildNum' (Branch _ l _) = 1 + (leftChildNum' l)

layout :: Tree a -> Tree (a, (Int, Int))
layout t = layout' t (rootPos t) 1 (2^((depth t) - 2))

layout' :: Tree a -> Int -> Int -> Int -> Tree (a, (Int, Int))
layout' Empty pos _ _ = Empty
layout' (Branch x l r) pos depth stride = Branch (x, (pos, depth)) newLeft newRight
        where newLeft = layout' l (pos - stride) (depth+1) (stride `div` 2)
              newRight = layout' r (pos + stride) (depth+1) (stride `div` 2)
