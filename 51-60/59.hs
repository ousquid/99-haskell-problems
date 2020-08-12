-- take 4 $ hbalTree 'x' 3

data Tree a = Empty | Branch a (Tree a) (Tree a)
              deriving (Show, Eq)
leaf x = Branch x Empty Empty

hbalTree :: a -> Int -> [Tree a]
hbalTree x 0 = [Empty]
hbalTree x 1 = [Branch x Empty Empty]
hbalTree x n = [Branch x left right | 
                    (lval, rval) <- [(n-1,n-1), (n-2,n-1), (n-1,n-2)],
                    left <- hbalTree x lval,
                    right <- hbalTree x rval]

hbaltree' :: a -> Int -> [Tree a]
hbaltree' x h = trees !! h
 where trees = [Empty] : [Branch x Empty Empty] :
               zipWith combine (tail trees) trees
       combine ts shortts = [Branch x l r |
               (ls, rs) <- [(shortts, ts), (ts, ts), (ts, shortts)],
               l <- ls, r <- rs]

