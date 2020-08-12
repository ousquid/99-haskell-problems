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

treeToString :: Tree Char -> String
treeToString Empty = ""
treeToString (Branch x l r) =  [x] ++ "(" ++ (treeToString l) ++ "," ++ (treeToString r) ++ ")"

stringToTree :: String -> Tree Char
stringToTree (x:xs) = head $ foldl foldingFunction [leaf x] xs
    where foldingFunction acc '(' = Empty:acc
          foldingFunction acc ',' = Empty:acc
          foldingFunction (r:l:(Branch x Empty Empty):acc) ')' = (Branch x l r):acc
          foldingFunction (Empty:acc) x = (leaf x):acc

