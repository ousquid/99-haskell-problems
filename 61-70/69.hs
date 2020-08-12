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

tree67 = Branch 'x' (Branch 'y' Empty Empty) (Branch 'a' Empty (Branch 'b' Empty Empty))

-- result: Branch 'a' (Branch 'b' (Branch 'd' Empty Empty) (Branch 'e' Empty Empty)) (Branch 'c' Empty (Branch 'f' (Branch 'g' Empty Empty) Empty))

ds2tree :: String -> Tree Char
ds2tree str = fst $ ds2tree' str

ds2tree' :: String -> (Tree Char, String)
ds2tree' (c:s)
    | c == '.' = (Empty, s)
    | otherwise = (Branch c ltree rtree, rres)
        where (ltree, lres) = ds2tree' s
              (rtree, rres) = ds2tree' lres
              
-- tree => (Char + ltree + rtree) | Empty

-- result: "xy..z0..."
tree2ds :: Tree Char -> String
tree2ds Empty = "."
tree2ds tree@(Branch a l r) =  [a] ++ tree2ds l ++ tree2ds r 
