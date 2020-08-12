
data Tree a = Empty | Branch a (Tree a) (Tree a)
              deriving (Show, Eq)
leaf x = Branch x Empty Empty

minNodes :: Int -> Int
minNodes x
 | x == 1 = 1
 | x == 2 = 2
 | otherwise = 1 + (minNodes (x-1)) + (minNodes (x-2))
 
maxNodes :: Int -> Int
maxNodes x = 2^x - 1

maxHeight :: Int -> Int
maxHeight x = length $ takeWhile (<=x) $ map minNodes [1..]

minHeight :: Int -> Int
minHeight x = 1 + (length $ takeWhile (<x) $ map maxNodes [1..])

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

hbalTreeNodes :: a -> Int -> [Tree a]
hbalTreeNodes c n = filter (\x -> n == countNode x) $ concatMap (hbalTree c) [(minHeight n)..(maxHeight n)]

countNode :: Tree a -> Int
countNode Empty = 0
countNode (Branch n l r) = (countNode l) + (countNode r) + 1